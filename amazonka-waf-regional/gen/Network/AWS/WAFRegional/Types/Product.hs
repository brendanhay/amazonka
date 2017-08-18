{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.Product where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.WAFRegional.Types.Sum

-- | The @ActivatedRule@ object in an 'UpdateWebACL' request specifies a @Rule@ that you want to insert or delete, the priority of the @Rule@ in the @WebACL@ , and the action that you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@ , @BLOCK@ , or @COUNT@ ).
--
--
-- To specify whether to insert or delete a @Rule@ , use the @Action@ parameter in the 'WebACLUpdate' data type.
--
--
-- /See:/ 'activatedRule' smart constructor.
data ActivatedRule = ActivatedRule'
    { _arType     :: !(Maybe WafRuleType)
    , _arPriority :: !Int
    , _arRuleId   :: !Text
    , _arAction   :: !WafAction
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ActivatedRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arType' - The rule type, either @REGULAR@ , as defined by 'Rule' , or @RATE_BASED@ , as defined by 'RateBasedRule' . The default is REGULAR. Although this field is optional, be aware that if you try to add a RATE_BASED rule to a web ACL without setting the type, the 'UpdateWebACL' request will fail because the request tries to add a REGULAR rule with the specified ID, which does not exist.
--
-- * 'arPriority' - Specifies the order in which the @Rules@ in a @WebACL@ are evaluated. Rules with a lower value for @Priority@ are evaluated before @Rules@ with a higher value. The value must be a unique integer. If you add multiple @Rules@ to a @WebACL@ , the values don't need to be consecutive.
--
-- * 'arRuleId' - The @RuleId@ for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete a one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ). @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
--
-- * 'arAction' - Specifies the action that CloudFront or AWS WAF takes when a web request matches the conditions in the @Rule@ . Valid values for @Action@ include the following:     * @ALLOW@ : CloudFront responds with the requested object.     * @BLOCK@ : CloudFront responds with an HTTP 403 (Forbidden) status code.     * @COUNT@ : AWS WAF increments a counter of requests that match the conditions in the rule and then continues to inspect the web request based on the remaining rules in the web ACL.
activatedRule
    :: Int -- ^ 'arPriority'
    -> Text -- ^ 'arRuleId'
    -> WafAction -- ^ 'arAction'
    -> ActivatedRule
activatedRule pPriority_ pRuleId_ pAction_ =
    ActivatedRule'
    { _arType = Nothing
    , _arPriority = pPriority_
    , _arRuleId = pRuleId_
    , _arAction = pAction_
    }

-- | The rule type, either @REGULAR@ , as defined by 'Rule' , or @RATE_BASED@ , as defined by 'RateBasedRule' . The default is REGULAR. Although this field is optional, be aware that if you try to add a RATE_BASED rule to a web ACL without setting the type, the 'UpdateWebACL' request will fail because the request tries to add a REGULAR rule with the specified ID, which does not exist.
arType :: Lens' ActivatedRule (Maybe WafRuleType)
arType = lens _arType (\ s a -> s{_arType = a});

-- | Specifies the order in which the @Rules@ in a @WebACL@ are evaluated. Rules with a lower value for @Priority@ are evaluated before @Rules@ with a higher value. The value must be a unique integer. If you add multiple @Rules@ to a @WebACL@ , the values don't need to be consecutive.
arPriority :: Lens' ActivatedRule Int
arPriority = lens _arPriority (\ s a -> s{_arPriority = a});

-- | The @RuleId@ for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete a one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ). @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
arRuleId :: Lens' ActivatedRule Text
arRuleId = lens _arRuleId (\ s a -> s{_arRuleId = a});

-- | Specifies the action that CloudFront or AWS WAF takes when a web request matches the conditions in the @Rule@ . Valid values for @Action@ include the following:     * @ALLOW@ : CloudFront responds with the requested object.     * @BLOCK@ : CloudFront responds with an HTTP 403 (Forbidden) status code.     * @COUNT@ : AWS WAF increments a counter of requests that match the conditions in the rule and then continues to inspect the web request based on the remaining rules in the web ACL.
arAction :: Lens' ActivatedRule WafAction
arAction = lens _arAction (\ s a -> s{_arAction = a});

instance FromJSON ActivatedRule where
        parseJSON
          = withObject "ActivatedRule"
              (\ x ->
                 ActivatedRule' <$>
                   (x .:? "Type") <*> (x .: "Priority") <*>
                     (x .: "RuleId")
                     <*> (x .: "Action"))

instance Hashable ActivatedRule

instance NFData ActivatedRule

instance ToJSON ActivatedRule where
        toJSON ActivatedRule'{..}
          = object
              (catMaybes
                 [("Type" .=) <$> _arType,
                  Just ("Priority" .= _arPriority),
                  Just ("RuleId" .= _arRuleId),
                  Just ("Action" .= _arAction)])

-- | In a 'GetByteMatchSet' request, @ByteMatchSet@ is a complex type that contains the @ByteMatchSetId@ and @Name@ of a @ByteMatchSet@ , and the values that you specified when you updated the @ByteMatchSet@ .
--
--
-- A complex type that contains @ByteMatchTuple@ objects, which specify the parts of web requests that you want AWS WAF to inspect and the values that you want AWS WAF to search for. If a @ByteMatchSet@ contains more than one @ByteMatchTuple@ object, a request needs to match the settings in only one @ByteMatchTuple@ to be considered a match.
--
--
-- /See:/ 'byteMatchSet' smart constructor.
data ByteMatchSet = ByteMatchSet'
    { _bmsName            :: !(Maybe Text)
    , _bmsByteMatchSetId  :: !Text
    , _bmsByteMatchTuples :: ![ByteMatchTuple]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ByteMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmsName' - A friendly name or description of the 'ByteMatchSet' . You can't change @Name@ after you create a @ByteMatchSet@ .
--
-- * 'bmsByteMatchSetId' - The @ByteMatchSetId@ for a @ByteMatchSet@ . You use @ByteMatchSetId@ to get information about a @ByteMatchSet@ (see 'GetByteMatchSet' ), update a @ByteMatchSet@ (see 'UpdateByteMatchSet' ), insert a @ByteMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @ByteMatchSet@ from AWS WAF (see 'DeleteByteMatchSet' ). @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
--
-- * 'bmsByteMatchTuples' - Specifies the bytes (typically a string that corresponds with ASCII characters) that you want AWS WAF to search for in web requests, the location in requests that you want AWS WAF to search, and other settings.
byteMatchSet
    :: Text -- ^ 'bmsByteMatchSetId'
    -> ByteMatchSet
byteMatchSet pByteMatchSetId_ =
    ByteMatchSet'
    { _bmsName = Nothing
    , _bmsByteMatchSetId = pByteMatchSetId_
    , _bmsByteMatchTuples = mempty
    }

-- | A friendly name or description of the 'ByteMatchSet' . You can't change @Name@ after you create a @ByteMatchSet@ .
bmsName :: Lens' ByteMatchSet (Maybe Text)
bmsName = lens _bmsName (\ s a -> s{_bmsName = a});

-- | The @ByteMatchSetId@ for a @ByteMatchSet@ . You use @ByteMatchSetId@ to get information about a @ByteMatchSet@ (see 'GetByteMatchSet' ), update a @ByteMatchSet@ (see 'UpdateByteMatchSet' ), insert a @ByteMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @ByteMatchSet@ from AWS WAF (see 'DeleteByteMatchSet' ). @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
bmsByteMatchSetId :: Lens' ByteMatchSet Text
bmsByteMatchSetId = lens _bmsByteMatchSetId (\ s a -> s{_bmsByteMatchSetId = a});

-- | Specifies the bytes (typically a string that corresponds with ASCII characters) that you want AWS WAF to search for in web requests, the location in requests that you want AWS WAF to search, and other settings.
bmsByteMatchTuples :: Lens' ByteMatchSet [ByteMatchTuple]
bmsByteMatchTuples = lens _bmsByteMatchTuples (\ s a -> s{_bmsByteMatchTuples = a}) . _Coerce;

instance FromJSON ByteMatchSet where
        parseJSON
          = withObject "ByteMatchSet"
              (\ x ->
                 ByteMatchSet' <$>
                   (x .:? "Name") <*> (x .: "ByteMatchSetId") <*>
                     (x .:? "ByteMatchTuples" .!= mempty))

instance Hashable ByteMatchSet

instance NFData ByteMatchSet

-- | Returned by 'ListByteMatchSets' . Each @ByteMatchSetSummary@ object includes the @Name@ and @ByteMatchSetId@ for one 'ByteMatchSet' .
--
--
--
-- /See:/ 'byteMatchSetSummary' smart constructor.
data ByteMatchSetSummary = ByteMatchSetSummary'
    { _bmssByteMatchSetId :: !Text
    , _bmssName           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ByteMatchSetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmssByteMatchSetId' - The @ByteMatchSetId@ for a @ByteMatchSet@ . You use @ByteMatchSetId@ to get information about a @ByteMatchSet@ , update a @ByteMatchSet@ , remove a @ByteMatchSet@ from a @Rule@ , and delete a @ByteMatchSet@ from AWS WAF. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
--
-- * 'bmssName' - A friendly name or description of the 'ByteMatchSet' . You can't change @Name@ after you create a @ByteMatchSet@ .
byteMatchSetSummary
    :: Text -- ^ 'bmssByteMatchSetId'
    -> Text -- ^ 'bmssName'
    -> ByteMatchSetSummary
byteMatchSetSummary pByteMatchSetId_ pName_ =
    ByteMatchSetSummary'
    { _bmssByteMatchSetId = pByteMatchSetId_
    , _bmssName = pName_
    }

-- | The @ByteMatchSetId@ for a @ByteMatchSet@ . You use @ByteMatchSetId@ to get information about a @ByteMatchSet@ , update a @ByteMatchSet@ , remove a @ByteMatchSet@ from a @Rule@ , and delete a @ByteMatchSet@ from AWS WAF. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
bmssByteMatchSetId :: Lens' ByteMatchSetSummary Text
bmssByteMatchSetId = lens _bmssByteMatchSetId (\ s a -> s{_bmssByteMatchSetId = a});

-- | A friendly name or description of the 'ByteMatchSet' . You can't change @Name@ after you create a @ByteMatchSet@ .
bmssName :: Lens' ByteMatchSetSummary Text
bmssName = lens _bmssName (\ s a -> s{_bmssName = a});

instance FromJSON ByteMatchSetSummary where
        parseJSON
          = withObject "ByteMatchSetSummary"
              (\ x ->
                 ByteMatchSetSummary' <$>
                   (x .: "ByteMatchSetId") <*> (x .: "Name"))

instance Hashable ByteMatchSetSummary

instance NFData ByteMatchSetSummary

-- | In an 'UpdateByteMatchSet' request, @ByteMatchSetUpdate@ specifies whether to insert or delete a 'ByteMatchTuple' and includes the settings for the @ByteMatchTuple@ .
--
--
--
-- /See:/ 'byteMatchSetUpdate' smart constructor.
data ByteMatchSetUpdate = ByteMatchSetUpdate'
    { _bmsuAction         :: !ChangeAction
    , _bmsuByteMatchTuple :: !ByteMatchTuple
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ByteMatchSetUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmsuAction' - Specifies whether to insert or delete a 'ByteMatchTuple' .
--
-- * 'bmsuByteMatchTuple' - Information about the part of a web request that you want AWS WAF to inspect and the value that you want AWS WAF to search for. If you specify @DELETE@ for the value of @Action@ , the @ByteMatchTuple@ values must exactly match the values in the @ByteMatchTuple@ that you want to delete from the @ByteMatchSet@ .
byteMatchSetUpdate
    :: ChangeAction -- ^ 'bmsuAction'
    -> ByteMatchTuple -- ^ 'bmsuByteMatchTuple'
    -> ByteMatchSetUpdate
byteMatchSetUpdate pAction_ pByteMatchTuple_ =
    ByteMatchSetUpdate'
    { _bmsuAction = pAction_
    , _bmsuByteMatchTuple = pByteMatchTuple_
    }

-- | Specifies whether to insert or delete a 'ByteMatchTuple' .
bmsuAction :: Lens' ByteMatchSetUpdate ChangeAction
bmsuAction = lens _bmsuAction (\ s a -> s{_bmsuAction = a});

-- | Information about the part of a web request that you want AWS WAF to inspect and the value that you want AWS WAF to search for. If you specify @DELETE@ for the value of @Action@ , the @ByteMatchTuple@ values must exactly match the values in the @ByteMatchTuple@ that you want to delete from the @ByteMatchSet@ .
bmsuByteMatchTuple :: Lens' ByteMatchSetUpdate ByteMatchTuple
bmsuByteMatchTuple = lens _bmsuByteMatchTuple (\ s a -> s{_bmsuByteMatchTuple = a});

instance Hashable ByteMatchSetUpdate

instance NFData ByteMatchSetUpdate

instance ToJSON ByteMatchSetUpdate where
        toJSON ByteMatchSetUpdate'{..}
          = object
              (catMaybes
                 [Just ("Action" .= _bmsuAction),
                  Just ("ByteMatchTuple" .= _bmsuByteMatchTuple)])

-- | The bytes (typically a string that corresponds with ASCII characters) that you want AWS WAF to search for in web requests, the location in requests that you want AWS WAF to search, and other settings.
--
--
--
-- /See:/ 'byteMatchTuple' smart constructor.
data ByteMatchTuple = ByteMatchTuple'
    { _bmtFieldToMatch         :: !FieldToMatch
    , _bmtTargetString         :: !Base64
    , _bmtTextTransformation   :: !TextTransformation
    , _bmtPositionalConstraint :: !PositionalConstraint
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ByteMatchTuple' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmtFieldToMatch' - The part of a web request that you want AWS WAF to search, such as a specified header or a query string. For more information, see 'FieldToMatch' .
--
-- * 'bmtTargetString' - The value that you want AWS WAF to search for. AWS WAF searches for the specified string in the part of web requests that you specified in @FieldToMatch@ . The maximum length of the value is 50 bytes. Valid values depend on the values that you specified for @FieldToMatch@ :     * @HEADER@ : The value that you want AWS WAF to search for in the request header that you specified in 'FieldToMatch' , for example, the value of the @User-Agent@ or @Referer@ header.     * @METHOD@ : The HTTP method, which indicates the type of operation specified in the request. CloudFront supports the following methods: @DELETE@ , @GET@ , @HEAD@ , @OPTIONS@ , @PATCH@ , @POST@ , and @PUT@ .     * @QUERY_STRING@ : The value that you want AWS WAF to search for in the query string, which is the part of a URL that appears after a @?@ character.     * @URI@ : The value that you want AWS WAF to search for in the part of a URL that identifies a resource, for example, @/images/daily-ad.jpg@ .     * @BODY@ : The part of a request that contains any additional data that you want to send to your web server as the HTTP request body, such as data from a form. The request body immediately follows the request headers. Note that only the first @8192@ bytes of the request body are forwarded to AWS WAF for inspection. To allow or block requests based on the length of the body, you can create a size constraint set. For more information, see 'CreateSizeConstraintSet' .  If @TargetString@ includes alphabetic characters A-Z and a-z, note that the value is case sensitive. __If you're using the AWS WAF API__  Specify a base64-encoded version of the value. The maximum length of the value before you base64-encode it is 50 bytes. For example, suppose the value of @Type@ is @HEADER@ and the value of @Data@ is @User-Agent@ . If you want to search the @User-Agent@ header for the value @BadBot@ , you base64-encode @BadBot@ using MIME base64 encoding and include the resulting value, @QmFkQm90@ , in the value of @TargetString@ . __If you're using the AWS CLI or one of the AWS SDKs__  The value that you want AWS WAF to search for. The SDK automatically base64 encodes the value.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'bmtTextTransformation' - Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @TargetString@ before inspecting a request for a match. __CMD_LINE__  When you're concerned that attackers are injecting an operating system commandline command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:     * Delete the following characters: \ " ' ^     * Delete spaces before the following characters: / (     * Replace the following characters with a space: , ;     * Replace multiple spaces with one space     * Convert uppercase letters (A-Z) to lowercase (a-z) __COMPRESS_WHITE_SPACE__  Use this option to replace the following characters with a space character (decimal 32):     * \f, formfeed, decimal 12     * \t, tab, decimal 9     * \n, newline, decimal 10     * \r, carriage return, decimal 13     * \v, vertical tab, decimal 11     * non-breaking space, decimal 160 @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space. __HTML_ENTITY_DECODE__  Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:     * Replaces @(ampersand)quot;@ with @"@      * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160     * Replaces @(ampersand)lt;@ with a "less than" symbol     * Replaces @(ampersand)gt;@ with @>@      * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters __LOWERCASE__  Use this option to convert uppercase letters (A-Z) to lowercase (a-z). __URL_DECODE__  Use this option to decode a URL-encoded value. __NONE__  Specify @NONE@ if you don't want to perform any text transformations.
--
-- * 'bmtPositionalConstraint' - Within the portion of a web request that you want to search (for example, in the query string, if any), specify where you want AWS WAF to search. Valid values include the following: __CONTAINS__  The specified part of the web request must include the value of @TargetString@ , but the location doesn't matter. __CONTAINS_WORD__  The specified part of the web request must include the value of @TargetString@ , and @TargetString@ must contain only alphanumeric characters or underscore (A-Z, a-z, 0-9, or _). In addition, @TargetString@ must be a word, which means one of the following:     * @TargetString@ exactly matches the value of the specified part of the web request, such as the value of a header.     * @TargetString@ is at the beginning of the specified part of the web request and is followed by a character other than an alphanumeric character or underscore (_), for example, @BadBot;@ .     * @TargetString@ is at the end of the specified part of the web request and is preceded by a character other than an alphanumeric character or underscore (_), for example, @;BadBot@ .     * @TargetString@ is in the middle of the specified part of the web request and is preceded and followed by characters other than alphanumeric characters or underscore (_), for example, @-BadBot;@ . __EXACTLY__  The value of the specified part of the web request must exactly match the value of @TargetString@ . __STARTS_WITH__  The value of @TargetString@ must appear at the beginning of the specified part of the web request. __ENDS_WITH__  The value of @TargetString@ must appear at the end of the specified part of the web request.
byteMatchTuple
    :: FieldToMatch -- ^ 'bmtFieldToMatch'
    -> ByteString -- ^ 'bmtTargetString'
    -> TextTransformation -- ^ 'bmtTextTransformation'
    -> PositionalConstraint -- ^ 'bmtPositionalConstraint'
    -> ByteMatchTuple
byteMatchTuple pFieldToMatch_ pTargetString_ pTextTransformation_ pPositionalConstraint_ =
    ByteMatchTuple'
    { _bmtFieldToMatch = pFieldToMatch_
    , _bmtTargetString = _Base64 # pTargetString_
    , _bmtTextTransformation = pTextTransformation_
    , _bmtPositionalConstraint = pPositionalConstraint_
    }

-- | The part of a web request that you want AWS WAF to search, such as a specified header or a query string. For more information, see 'FieldToMatch' .
bmtFieldToMatch :: Lens' ByteMatchTuple FieldToMatch
bmtFieldToMatch = lens _bmtFieldToMatch (\ s a -> s{_bmtFieldToMatch = a});

-- | The value that you want AWS WAF to search for. AWS WAF searches for the specified string in the part of web requests that you specified in @FieldToMatch@ . The maximum length of the value is 50 bytes. Valid values depend on the values that you specified for @FieldToMatch@ :     * @HEADER@ : The value that you want AWS WAF to search for in the request header that you specified in 'FieldToMatch' , for example, the value of the @User-Agent@ or @Referer@ header.     * @METHOD@ : The HTTP method, which indicates the type of operation specified in the request. CloudFront supports the following methods: @DELETE@ , @GET@ , @HEAD@ , @OPTIONS@ , @PATCH@ , @POST@ , and @PUT@ .     * @QUERY_STRING@ : The value that you want AWS WAF to search for in the query string, which is the part of a URL that appears after a @?@ character.     * @URI@ : The value that you want AWS WAF to search for in the part of a URL that identifies a resource, for example, @/images/daily-ad.jpg@ .     * @BODY@ : The part of a request that contains any additional data that you want to send to your web server as the HTTP request body, such as data from a form. The request body immediately follows the request headers. Note that only the first @8192@ bytes of the request body are forwarded to AWS WAF for inspection. To allow or block requests based on the length of the body, you can create a size constraint set. For more information, see 'CreateSizeConstraintSet' .  If @TargetString@ includes alphabetic characters A-Z and a-z, note that the value is case sensitive. __If you're using the AWS WAF API__  Specify a base64-encoded version of the value. The maximum length of the value before you base64-encode it is 50 bytes. For example, suppose the value of @Type@ is @HEADER@ and the value of @Data@ is @User-Agent@ . If you want to search the @User-Agent@ header for the value @BadBot@ , you base64-encode @BadBot@ using MIME base64 encoding and include the resulting value, @QmFkQm90@ , in the value of @TargetString@ . __If you're using the AWS CLI or one of the AWS SDKs__  The value that you want AWS WAF to search for. The SDK automatically base64 encodes the value.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
bmtTargetString :: Lens' ByteMatchTuple ByteString
bmtTargetString = lens _bmtTargetString (\ s a -> s{_bmtTargetString = a}) . _Base64;

-- | Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @TargetString@ before inspecting a request for a match. __CMD_LINE__  When you're concerned that attackers are injecting an operating system commandline command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:     * Delete the following characters: \ " ' ^     * Delete spaces before the following characters: / (     * Replace the following characters with a space: , ;     * Replace multiple spaces with one space     * Convert uppercase letters (A-Z) to lowercase (a-z) __COMPRESS_WHITE_SPACE__  Use this option to replace the following characters with a space character (decimal 32):     * \f, formfeed, decimal 12     * \t, tab, decimal 9     * \n, newline, decimal 10     * \r, carriage return, decimal 13     * \v, vertical tab, decimal 11     * non-breaking space, decimal 160 @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space. __HTML_ENTITY_DECODE__  Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:     * Replaces @(ampersand)quot;@ with @"@      * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160     * Replaces @(ampersand)lt;@ with a "less than" symbol     * Replaces @(ampersand)gt;@ with @>@      * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters __LOWERCASE__  Use this option to convert uppercase letters (A-Z) to lowercase (a-z). __URL_DECODE__  Use this option to decode a URL-encoded value. __NONE__  Specify @NONE@ if you don't want to perform any text transformations.
bmtTextTransformation :: Lens' ByteMatchTuple TextTransformation
bmtTextTransformation = lens _bmtTextTransformation (\ s a -> s{_bmtTextTransformation = a});

-- | Within the portion of a web request that you want to search (for example, in the query string, if any), specify where you want AWS WAF to search. Valid values include the following: __CONTAINS__  The specified part of the web request must include the value of @TargetString@ , but the location doesn't matter. __CONTAINS_WORD__  The specified part of the web request must include the value of @TargetString@ , and @TargetString@ must contain only alphanumeric characters or underscore (A-Z, a-z, 0-9, or _). In addition, @TargetString@ must be a word, which means one of the following:     * @TargetString@ exactly matches the value of the specified part of the web request, such as the value of a header.     * @TargetString@ is at the beginning of the specified part of the web request and is followed by a character other than an alphanumeric character or underscore (_), for example, @BadBot;@ .     * @TargetString@ is at the end of the specified part of the web request and is preceded by a character other than an alphanumeric character or underscore (_), for example, @;BadBot@ .     * @TargetString@ is in the middle of the specified part of the web request and is preceded and followed by characters other than alphanumeric characters or underscore (_), for example, @-BadBot;@ . __EXACTLY__  The value of the specified part of the web request must exactly match the value of @TargetString@ . __STARTS_WITH__  The value of @TargetString@ must appear at the beginning of the specified part of the web request. __ENDS_WITH__  The value of @TargetString@ must appear at the end of the specified part of the web request.
bmtPositionalConstraint :: Lens' ByteMatchTuple PositionalConstraint
bmtPositionalConstraint = lens _bmtPositionalConstraint (\ s a -> s{_bmtPositionalConstraint = a});

instance FromJSON ByteMatchTuple where
        parseJSON
          = withObject "ByteMatchTuple"
              (\ x ->
                 ByteMatchTuple' <$>
                   (x .: "FieldToMatch") <*> (x .: "TargetString") <*>
                     (x .: "TextTransformation")
                     <*> (x .: "PositionalConstraint"))

instance Hashable ByteMatchTuple

instance NFData ByteMatchTuple

instance ToJSON ByteMatchTuple where
        toJSON ByteMatchTuple'{..}
          = object
              (catMaybes
                 [Just ("FieldToMatch" .= _bmtFieldToMatch),
                  Just ("TargetString" .= _bmtTargetString),
                  Just
                    ("TextTransformation" .= _bmtTextTransformation),
                  Just
                    ("PositionalConstraint" .=
                       _bmtPositionalConstraint)])

-- | Specifies where in a web request to look for @TargetString@ .
--
--
--
-- /See:/ 'fieldToMatch' smart constructor.
data FieldToMatch = FieldToMatch'
    { _ftmData :: !(Maybe Text)
    , _ftmType :: !MatchFieldType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FieldToMatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ftmData' - When the value of @Type@ is @HEADER@ , enter the name of the header that you want AWS WAF to search, for example, @User-Agent@ or @Referer@ . If the value of @Type@ is any other value, omit @Data@ . The name of the header is not case sensitive.
--
-- * 'ftmType' - The part of the web request that you want AWS WAF to search for a specified string. Parts of a request that you can search include the following:     * @HEADER@ : A specified request header, for example, the value of the @User-Agent@ or @Referer@ header. If you choose @HEADER@ for the type, specify the name of the header in @Data@ .     * @METHOD@ : The HTTP method, which indicated the type of operation that the request is asking the origin to perform. Amazon CloudFront supports the following methods: @DELETE@ , @GET@ , @HEAD@ , @OPTIONS@ , @PATCH@ , @POST@ , and @PUT@ .     * @QUERY_STRING@ : A query string, which is the part of a URL that appears after a @?@ character, if any.     * @URI@ : The part of a web request that identifies a resource, for example, @/images/daily-ad.jpg@ .     * @BODY@ : The part of a request that contains any additional data that you want to send to your web server as the HTTP request body, such as data from a form. The request body immediately follows the request headers. Note that only the first @8192@ bytes of the request body are forwarded to AWS WAF for inspection. To allow or block requests based on the length of the body, you can create a size constraint set. For more information, see 'CreateSizeConstraintSet' .
fieldToMatch
    :: MatchFieldType -- ^ 'ftmType'
    -> FieldToMatch
fieldToMatch pType_ =
    FieldToMatch'
    { _ftmData = Nothing
    , _ftmType = pType_
    }

-- | When the value of @Type@ is @HEADER@ , enter the name of the header that you want AWS WAF to search, for example, @User-Agent@ or @Referer@ . If the value of @Type@ is any other value, omit @Data@ . The name of the header is not case sensitive.
ftmData :: Lens' FieldToMatch (Maybe Text)
ftmData = lens _ftmData (\ s a -> s{_ftmData = a});

-- | The part of the web request that you want AWS WAF to search for a specified string. Parts of a request that you can search include the following:     * @HEADER@ : A specified request header, for example, the value of the @User-Agent@ or @Referer@ header. If you choose @HEADER@ for the type, specify the name of the header in @Data@ .     * @METHOD@ : The HTTP method, which indicated the type of operation that the request is asking the origin to perform. Amazon CloudFront supports the following methods: @DELETE@ , @GET@ , @HEAD@ , @OPTIONS@ , @PATCH@ , @POST@ , and @PUT@ .     * @QUERY_STRING@ : A query string, which is the part of a URL that appears after a @?@ character, if any.     * @URI@ : The part of a web request that identifies a resource, for example, @/images/daily-ad.jpg@ .     * @BODY@ : The part of a request that contains any additional data that you want to send to your web server as the HTTP request body, such as data from a form. The request body immediately follows the request headers. Note that only the first @8192@ bytes of the request body are forwarded to AWS WAF for inspection. To allow or block requests based on the length of the body, you can create a size constraint set. For more information, see 'CreateSizeConstraintSet' .
ftmType :: Lens' FieldToMatch MatchFieldType
ftmType = lens _ftmType (\ s a -> s{_ftmType = a});

instance FromJSON FieldToMatch where
        parseJSON
          = withObject "FieldToMatch"
              (\ x ->
                 FieldToMatch' <$> (x .:? "Data") <*> (x .: "Type"))

instance Hashable FieldToMatch

instance NFData FieldToMatch

instance ToJSON FieldToMatch where
        toJSON FieldToMatch'{..}
          = object
              (catMaybes
                 [("Data" .=) <$> _ftmData,
                  Just ("Type" .= _ftmType)])

-- | The response from a 'GetSampledRequests' request includes an @HTTPHeader@ complex type that appears as @Headers@ in the response syntax. @HTTPHeader@ contains the names and values of all of the headers that appear in one of the web requests that were returned by @GetSampledRequests@ .
--
--
--
-- /See:/ 'hTTPHeader' smart constructor.
data HTTPHeader = HTTPHeader'
    { _httphValue :: !(Maybe Text)
    , _httphName  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'HTTPHeader' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httphValue' - The value of one of the headers in the sampled web request.
--
-- * 'httphName' - The name of one of the headers in the sampled web request.
hTTPHeader
    :: HTTPHeader
hTTPHeader =
    HTTPHeader'
    { _httphValue = Nothing
    , _httphName = Nothing
    }

-- | The value of one of the headers in the sampled web request.
httphValue :: Lens' HTTPHeader (Maybe Text)
httphValue = lens _httphValue (\ s a -> s{_httphValue = a});

-- | The name of one of the headers in the sampled web request.
httphName :: Lens' HTTPHeader (Maybe Text)
httphName = lens _httphName (\ s a -> s{_httphName = a});

instance FromJSON HTTPHeader where
        parseJSON
          = withObject "HTTPHeader"
              (\ x ->
                 HTTPHeader' <$> (x .:? "Value") <*> (x .:? "Name"))

instance Hashable HTTPHeader

instance NFData HTTPHeader

-- | The response from a 'GetSampledRequests' request includes an @HTTPRequest@ complex type that appears as @Request@ in the response syntax. @HTTPRequest@ contains information about one of the web requests that were returned by @GetSampledRequests@ .
--
--
--
-- /See:/ 'hTTPRequest' smart constructor.
data HTTPRequest = HTTPRequest'
    { _httprHTTPVersion :: !(Maybe Text)
    , _httprCountry     :: !(Maybe Text)
    , _httprURI         :: !(Maybe Text)
    , _httprHeaders     :: !(Maybe [HTTPHeader])
    , _httprMethod      :: !(Maybe Text)
    , _httprClientIP    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'HTTPRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httprHTTPVersion' - The HTTP version specified in the sampled web request, for example, @HTTP/1.1@ .
--
-- * 'httprCountry' - The two-letter country code for the country that the request originated from. For a current list of country codes, see the Wikipedia entry <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO 3166-1 alpha-2> .
--
-- * 'httprURI' - The part of a web request that identifies the resource, for example, @/images/daily-ad.jpg@ .
--
-- * 'httprHeaders' - A complex type that contains two values for each header in the sampled web request: the name of the header and the value of the header.
--
-- * 'httprMethod' - The HTTP method specified in the sampled web request. CloudFront supports the following methods: @DELETE@ , @GET@ , @HEAD@ , @OPTIONS@ , @PATCH@ , @POST@ , and @PUT@ .
--
-- * 'httprClientIP' - The IP address that the request originated from. If the @WebACL@ is associated with a CloudFront distribution, this is the value of one of the following fields in CloudFront access logs:     * @c-ip@ , if the viewer did not use an HTTP proxy or a load balancer to send the request     * @x-forwarded-for@ , if the viewer did use an HTTP proxy or a load balancer to send the request
hTTPRequest
    :: HTTPRequest
hTTPRequest =
    HTTPRequest'
    { _httprHTTPVersion = Nothing
    , _httprCountry = Nothing
    , _httprURI = Nothing
    , _httprHeaders = Nothing
    , _httprMethod = Nothing
    , _httprClientIP = Nothing
    }

-- | The HTTP version specified in the sampled web request, for example, @HTTP/1.1@ .
httprHTTPVersion :: Lens' HTTPRequest (Maybe Text)
httprHTTPVersion = lens _httprHTTPVersion (\ s a -> s{_httprHTTPVersion = a});

-- | The two-letter country code for the country that the request originated from. For a current list of country codes, see the Wikipedia entry <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO 3166-1 alpha-2> .
httprCountry :: Lens' HTTPRequest (Maybe Text)
httprCountry = lens _httprCountry (\ s a -> s{_httprCountry = a});

-- | The part of a web request that identifies the resource, for example, @/images/daily-ad.jpg@ .
httprURI :: Lens' HTTPRequest (Maybe Text)
httprURI = lens _httprURI (\ s a -> s{_httprURI = a});

-- | A complex type that contains two values for each header in the sampled web request: the name of the header and the value of the header.
httprHeaders :: Lens' HTTPRequest [HTTPHeader]
httprHeaders = lens _httprHeaders (\ s a -> s{_httprHeaders = a}) . _Default . _Coerce;

-- | The HTTP method specified in the sampled web request. CloudFront supports the following methods: @DELETE@ , @GET@ , @HEAD@ , @OPTIONS@ , @PATCH@ , @POST@ , and @PUT@ .
httprMethod :: Lens' HTTPRequest (Maybe Text)
httprMethod = lens _httprMethod (\ s a -> s{_httprMethod = a});

-- | The IP address that the request originated from. If the @WebACL@ is associated with a CloudFront distribution, this is the value of one of the following fields in CloudFront access logs:     * @c-ip@ , if the viewer did not use an HTTP proxy or a load balancer to send the request     * @x-forwarded-for@ , if the viewer did use an HTTP proxy or a load balancer to send the request
httprClientIP :: Lens' HTTPRequest (Maybe Text)
httprClientIP = lens _httprClientIP (\ s a -> s{_httprClientIP = a});

instance FromJSON HTTPRequest where
        parseJSON
          = withObject "HTTPRequest"
              (\ x ->
                 HTTPRequest' <$>
                   (x .:? "HTTPVersion") <*> (x .:? "Country") <*>
                     (x .:? "URI")
                     <*> (x .:? "Headers" .!= mempty)
                     <*> (x .:? "Method")
                     <*> (x .:? "ClientIP"))

instance Hashable HTTPRequest

instance NFData HTTPRequest

-- | Contains one or more IP addresses or blocks of IP addresses specified in Classless Inter-Domain Routing (CIDR) notation. AWS WAF supports /8, /16, /24, and /32 IP address ranges for IPv4, and /24, /32, /48, /56, /64 and /128 for IPv6.
--
--
-- To specify an individual IP address, you specify the four-part IP address followed by a @/32@ , for example, 192.0.2.0/31. To block a range of IP addresses, you can specify a @/128@ , @/64@ , @/56@ , @/48@ , @/32@ , @/24@ , @/16@ , or @/8@ CIDR. For more information about CIDR notation, see the Wikipedia entry <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing> .
--
--
-- /See:/ 'ipSet' smart constructor.
data IPSet = IPSet'
    { _isName             :: !(Maybe Text)
    , _isIPSetId          :: !Text
    , _isIPSetDescriptors :: ![IPSetDescriptor]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IPSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isName' - A friendly name or description of the 'IPSet' . You can't change the name of an @IPSet@ after you create it.
--
-- * 'isIPSetId' - The @IPSetId@ for an @IPSet@ . You use @IPSetId@ to get information about an @IPSet@ (see 'GetIPSet' ), update an @IPSet@ (see 'UpdateIPSet' ), insert an @IPSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @IPSet@ from AWS WAF (see 'DeleteIPSet' ). @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
--
-- * 'isIPSetDescriptors' - The IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR notation) that web requests originate from. If the @WebACL@ is associated with a CloudFront distribution and the viewer did not use an HTTP proxy or a load balancer to send the request, this is the value of the c-ip field in the CloudFront access logs.
ipSet
    :: Text -- ^ 'isIPSetId'
    -> IPSet
ipSet pIPSetId_ =
    IPSet'
    { _isName = Nothing
    , _isIPSetId = pIPSetId_
    , _isIPSetDescriptors = mempty
    }

-- | A friendly name or description of the 'IPSet' . You can't change the name of an @IPSet@ after you create it.
isName :: Lens' IPSet (Maybe Text)
isName = lens _isName (\ s a -> s{_isName = a});

-- | The @IPSetId@ for an @IPSet@ . You use @IPSetId@ to get information about an @IPSet@ (see 'GetIPSet' ), update an @IPSet@ (see 'UpdateIPSet' ), insert an @IPSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @IPSet@ from AWS WAF (see 'DeleteIPSet' ). @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
isIPSetId :: Lens' IPSet Text
isIPSetId = lens _isIPSetId (\ s a -> s{_isIPSetId = a});

-- | The IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR notation) that web requests originate from. If the @WebACL@ is associated with a CloudFront distribution and the viewer did not use an HTTP proxy or a load balancer to send the request, this is the value of the c-ip field in the CloudFront access logs.
isIPSetDescriptors :: Lens' IPSet [IPSetDescriptor]
isIPSetDescriptors = lens _isIPSetDescriptors (\ s a -> s{_isIPSetDescriptors = a}) . _Coerce;

instance FromJSON IPSet where
        parseJSON
          = withObject "IPSet"
              (\ x ->
                 IPSet' <$>
                   (x .:? "Name") <*> (x .: "IPSetId") <*>
                     (x .:? "IPSetDescriptors" .!= mempty))

instance Hashable IPSet

instance NFData IPSet

-- | Specifies the IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR format) that web requests originate from.
--
--
--
-- /See:/ 'ipSetDescriptor' smart constructor.
data IPSetDescriptor = IPSetDescriptor'
    { _isdType  :: !IPSetDescriptorType
    , _isdValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IPSetDescriptor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isdType' - Specify @IPV4@ or @IPV6@ .
--
-- * 'isdValue' - Specify an IPv4 address by using CIDR notation. For example:     * To configure AWS WAF to allow, block, or count requests that originated from the IP address 192.0.2.44, specify @192.0.2.44/32@ .     * To configure AWS WAF to allow, block, or count requests that originated from IP addresses from 192.0.2.0 to 192.0.2.255, specify @192.0.2.0/24@ . For more information about CIDR notation, see the Wikipedia entry <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing> . Specify an IPv6 address by using CIDR notation. For example:     * To configure AWS WAF to allow, block, or count requests that originated from the IP address 1111:0000:0000:0000:0000:0000:0000:0111, specify @1111:0000:0000:0000:0000:0000:0000:0111/128@ .     * To configure AWS WAF to allow, block, or count requests that originated from IP addresses 1111:0000:0000:0000:0000:0000:0000:0000 to 1111:0000:0000:0000:ffff:ffff:ffff:ffff, specify @1111:0000:0000:0000:0000:0000:0000:0000/64@ .
ipSetDescriptor
    :: IPSetDescriptorType -- ^ 'isdType'
    -> Text -- ^ 'isdValue'
    -> IPSetDescriptor
ipSetDescriptor pType_ pValue_ =
    IPSetDescriptor'
    { _isdType = pType_
    , _isdValue = pValue_
    }

-- | Specify @IPV4@ or @IPV6@ .
isdType :: Lens' IPSetDescriptor IPSetDescriptorType
isdType = lens _isdType (\ s a -> s{_isdType = a});

-- | Specify an IPv4 address by using CIDR notation. For example:     * To configure AWS WAF to allow, block, or count requests that originated from the IP address 192.0.2.44, specify @192.0.2.44/32@ .     * To configure AWS WAF to allow, block, or count requests that originated from IP addresses from 192.0.2.0 to 192.0.2.255, specify @192.0.2.0/24@ . For more information about CIDR notation, see the Wikipedia entry <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing> . Specify an IPv6 address by using CIDR notation. For example:     * To configure AWS WAF to allow, block, or count requests that originated from the IP address 1111:0000:0000:0000:0000:0000:0000:0111, specify @1111:0000:0000:0000:0000:0000:0000:0111/128@ .     * To configure AWS WAF to allow, block, or count requests that originated from IP addresses 1111:0000:0000:0000:0000:0000:0000:0000 to 1111:0000:0000:0000:ffff:ffff:ffff:ffff, specify @1111:0000:0000:0000:0000:0000:0000:0000/64@ .
isdValue :: Lens' IPSetDescriptor Text
isdValue = lens _isdValue (\ s a -> s{_isdValue = a});

instance FromJSON IPSetDescriptor where
        parseJSON
          = withObject "IPSetDescriptor"
              (\ x ->
                 IPSetDescriptor' <$>
                   (x .: "Type") <*> (x .: "Value"))

instance Hashable IPSetDescriptor

instance NFData IPSetDescriptor

instance ToJSON IPSetDescriptor where
        toJSON IPSetDescriptor'{..}
          = object
              (catMaybes
                 [Just ("Type" .= _isdType),
                  Just ("Value" .= _isdValue)])

-- | Contains the identifier and the name of the @IPSet@ .
--
--
--
-- /See:/ 'ipSetSummary' smart constructor.
data IPSetSummary = IPSetSummary'
    { _issIPSetId :: !Text
    , _issName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IPSetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'issIPSetId' - The @IPSetId@ for an 'IPSet' . You can use @IPSetId@ in a 'GetIPSet' request to get detailed information about an 'IPSet' .
--
-- * 'issName' - A friendly name or description of the 'IPSet' . You can't change the name of an @IPSet@ after you create it.
ipSetSummary
    :: Text -- ^ 'issIPSetId'
    -> Text -- ^ 'issName'
    -> IPSetSummary
ipSetSummary pIPSetId_ pName_ =
    IPSetSummary'
    { _issIPSetId = pIPSetId_
    , _issName = pName_
    }

-- | The @IPSetId@ for an 'IPSet' . You can use @IPSetId@ in a 'GetIPSet' request to get detailed information about an 'IPSet' .
issIPSetId :: Lens' IPSetSummary Text
issIPSetId = lens _issIPSetId (\ s a -> s{_issIPSetId = a});

-- | A friendly name or description of the 'IPSet' . You can't change the name of an @IPSet@ after you create it.
issName :: Lens' IPSetSummary Text
issName = lens _issName (\ s a -> s{_issName = a});

instance FromJSON IPSetSummary where
        parseJSON
          = withObject "IPSetSummary"
              (\ x ->
                 IPSetSummary' <$> (x .: "IPSetId") <*> (x .: "Name"))

instance Hashable IPSetSummary

instance NFData IPSetSummary

-- | Specifies the type of update to perform to an 'IPSet' with 'UpdateIPSet' .
--
--
--
-- /See:/ 'ipSetUpdate' smart constructor.
data IPSetUpdate = IPSetUpdate'
    { _isuAction          :: !ChangeAction
    , _isuIPSetDescriptor :: !IPSetDescriptor
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IPSetUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isuAction' - Specifies whether to insert or delete an IP address with 'UpdateIPSet' .
--
-- * 'isuIPSetDescriptor' - The IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR notation) that web requests originate from.
ipSetUpdate
    :: ChangeAction -- ^ 'isuAction'
    -> IPSetDescriptor -- ^ 'isuIPSetDescriptor'
    -> IPSetUpdate
ipSetUpdate pAction_ pIPSetDescriptor_ =
    IPSetUpdate'
    { _isuAction = pAction_
    , _isuIPSetDescriptor = pIPSetDescriptor_
    }

-- | Specifies whether to insert or delete an IP address with 'UpdateIPSet' .
isuAction :: Lens' IPSetUpdate ChangeAction
isuAction = lens _isuAction (\ s a -> s{_isuAction = a});

-- | The IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR notation) that web requests originate from.
isuIPSetDescriptor :: Lens' IPSetUpdate IPSetDescriptor
isuIPSetDescriptor = lens _isuIPSetDescriptor (\ s a -> s{_isuIPSetDescriptor = a});

instance Hashable IPSetUpdate

instance NFData IPSetUpdate

instance ToJSON IPSetUpdate where
        toJSON IPSetUpdate'{..}
          = object
              (catMaybes
                 [Just ("Action" .= _isuAction),
                  Just ("IPSetDescriptor" .= _isuIPSetDescriptor)])

-- | Specifies the 'ByteMatchSet' , 'IPSet' , 'SqlInjectionMatchSet' , 'XssMatchSet' , and 'SizeConstraintSet' objects that you want to add to a @Rule@ and, for each object, indicates whether you want to negate the settings, for example, requests that do NOT originate from the IP address 192.0.2.44.
--
--
--
-- /See:/ 'predicate' smart constructor.
data Predicate = Predicate'
    { _pNegated :: !Bool
    , _pType    :: !PredicateType
    , _pDataId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Predicate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pNegated' - Set @Negated@ to @False@ if you want AWS WAF to allow, block, or count requests based on the settings in the specified 'ByteMatchSet' , 'IPSet' , 'SqlInjectionMatchSet' , 'XssMatchSet' , or 'SizeConstraintSet' . For example, if an @IPSet@ includes the IP address @192.0.2.44@ , AWS WAF will allow or block requests based on that IP address. Set @Negated@ to @True@ if you want AWS WAF to allow or block a request based on the negation of the settings in the 'ByteMatchSet' , 'IPSet' , 'SqlInjectionMatchSet' , 'XssMatchSet' , or 'SizeConstraintSet' . For example, if an @IPSet@ includes the IP address @192.0.2.44@ , AWS WAF will allow, block, or count requests based on all IP addresses /except/ @192.0.2.44@ .
--
-- * 'pType' - The type of predicate in a @Rule@ , such as @ByteMatchSet@ or @IPSet@ .
--
-- * 'pDataId' - A unique identifier for a predicate in a @Rule@ , such as @ByteMatchSetId@ or @IPSetId@ . The ID is returned by the corresponding @Create@ or @List@ command.
predicate
    :: Bool -- ^ 'pNegated'
    -> PredicateType -- ^ 'pType'
    -> Text -- ^ 'pDataId'
    -> Predicate
predicate pNegated_ pType_ pDataId_ =
    Predicate'
    { _pNegated = pNegated_
    , _pType = pType_
    , _pDataId = pDataId_
    }

-- | Set @Negated@ to @False@ if you want AWS WAF to allow, block, or count requests based on the settings in the specified 'ByteMatchSet' , 'IPSet' , 'SqlInjectionMatchSet' , 'XssMatchSet' , or 'SizeConstraintSet' . For example, if an @IPSet@ includes the IP address @192.0.2.44@ , AWS WAF will allow or block requests based on that IP address. Set @Negated@ to @True@ if you want AWS WAF to allow or block a request based on the negation of the settings in the 'ByteMatchSet' , 'IPSet' , 'SqlInjectionMatchSet' , 'XssMatchSet' , or 'SizeConstraintSet' . For example, if an @IPSet@ includes the IP address @192.0.2.44@ , AWS WAF will allow, block, or count requests based on all IP addresses /except/ @192.0.2.44@ .
pNegated :: Lens' Predicate Bool
pNegated = lens _pNegated (\ s a -> s{_pNegated = a});

-- | The type of predicate in a @Rule@ , such as @ByteMatchSet@ or @IPSet@ .
pType :: Lens' Predicate PredicateType
pType = lens _pType (\ s a -> s{_pType = a});

-- | A unique identifier for a predicate in a @Rule@ , such as @ByteMatchSetId@ or @IPSetId@ . The ID is returned by the corresponding @Create@ or @List@ command.
pDataId :: Lens' Predicate Text
pDataId = lens _pDataId (\ s a -> s{_pDataId = a});

instance FromJSON Predicate where
        parseJSON
          = withObject "Predicate"
              (\ x ->
                 Predicate' <$>
                   (x .: "Negated") <*> (x .: "Type") <*>
                     (x .: "DataId"))

instance Hashable Predicate

instance NFData Predicate

instance ToJSON Predicate where
        toJSON Predicate'{..}
          = object
              (catMaybes
                 [Just ("Negated" .= _pNegated),
                  Just ("Type" .= _pType),
                  Just ("DataId" .= _pDataId)])

-- | A @RateBasedRule@ is identical to a regular 'Rule' , with one addition: a @RateBasedRule@ counts the number of requests that arrive from a specified IP address every five minutes. For example, based on recent requests that you've seen from an attacker, you might create a @RateBasedRule@ that includes the following conditions:
--
--
--     * The requests come from 192.0.2.44.
--
--     * They contain the value @BadBot@ in the @User-Agent@ header.
--
--
--
-- In the rule, you also define the rate limit as 15,000.
--
-- Requests that meet both of these conditions and exceed 15,000 requests every five minutes trigger the rule's action (block or count), which is defined in the web ACL.
--
--
-- /See:/ 'rateBasedRule' smart constructor.
data RateBasedRule = RateBasedRule'
    { _rbrMetricName      :: !(Maybe Text)
    , _rbrName            :: !(Maybe Text)
    , _rbrRuleId          :: !Text
    , _rbrMatchPredicates :: ![Predicate]
    , _rbrRateKey         :: !RateKey
    , _rbrRateLimit       :: !Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RateBasedRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rbrMetricName' - A friendly name or description for the metrics for a @RateBasedRule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9); the name can't contain whitespace. You can't change the name of the metric after you create the @RateBasedRule@ .
--
-- * 'rbrName' - A friendly name or description for a @RateBasedRule@ . You can't change the name of a @RateBasedRule@ after you create it.
--
-- * 'rbrRuleId' - A unique identifier for a @RateBasedRule@ . You use @RuleId@ to get more information about a @RateBasedRule@ (see 'GetRateBasedRule' ), update a @RateBasedRule@ (see 'UpdateRateBasedRule' ), insert a @RateBasedRule@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @RateBasedRule@ from AWS WAF (see 'DeleteRateBasedRule' ).
--
-- * 'rbrMatchPredicates' - The @Predicates@ object contains one @Predicate@ element for each 'ByteMatchSet' , 'IPSet' , or 'SqlInjectionMatchSet' object that you want to include in a @RateBasedRule@ .
--
-- * 'rbrRateKey' - The field that AWS WAF uses to determine if requests are likely arriving from single source and thus subject to rate monitoring. The only valid value for @RateKey@ is @IP@ . @IP@ indicates that requests arriving from the same IP address are subject to the @RateLimit@ that is specified in the @RateBasedRule@ .
--
-- * 'rbrRateLimit' - The maximum number of requests, which have an identical value in the field specified by the @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
rateBasedRule
    :: Text -- ^ 'rbrRuleId'
    -> RateKey -- ^ 'rbrRateKey'
    -> Natural -- ^ 'rbrRateLimit'
    -> RateBasedRule
rateBasedRule pRuleId_ pRateKey_ pRateLimit_ =
    RateBasedRule'
    { _rbrMetricName = Nothing
    , _rbrName = Nothing
    , _rbrRuleId = pRuleId_
    , _rbrMatchPredicates = mempty
    , _rbrRateKey = pRateKey_
    , _rbrRateLimit = _Nat # pRateLimit_
    }

-- | A friendly name or description for the metrics for a @RateBasedRule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9); the name can't contain whitespace. You can't change the name of the metric after you create the @RateBasedRule@ .
rbrMetricName :: Lens' RateBasedRule (Maybe Text)
rbrMetricName = lens _rbrMetricName (\ s a -> s{_rbrMetricName = a});

-- | A friendly name or description for a @RateBasedRule@ . You can't change the name of a @RateBasedRule@ after you create it.
rbrName :: Lens' RateBasedRule (Maybe Text)
rbrName = lens _rbrName (\ s a -> s{_rbrName = a});

-- | A unique identifier for a @RateBasedRule@ . You use @RuleId@ to get more information about a @RateBasedRule@ (see 'GetRateBasedRule' ), update a @RateBasedRule@ (see 'UpdateRateBasedRule' ), insert a @RateBasedRule@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @RateBasedRule@ from AWS WAF (see 'DeleteRateBasedRule' ).
rbrRuleId :: Lens' RateBasedRule Text
rbrRuleId = lens _rbrRuleId (\ s a -> s{_rbrRuleId = a});

-- | The @Predicates@ object contains one @Predicate@ element for each 'ByteMatchSet' , 'IPSet' , or 'SqlInjectionMatchSet' object that you want to include in a @RateBasedRule@ .
rbrMatchPredicates :: Lens' RateBasedRule [Predicate]
rbrMatchPredicates = lens _rbrMatchPredicates (\ s a -> s{_rbrMatchPredicates = a}) . _Coerce;

-- | The field that AWS WAF uses to determine if requests are likely arriving from single source and thus subject to rate monitoring. The only valid value for @RateKey@ is @IP@ . @IP@ indicates that requests arriving from the same IP address are subject to the @RateLimit@ that is specified in the @RateBasedRule@ .
rbrRateKey :: Lens' RateBasedRule RateKey
rbrRateKey = lens _rbrRateKey (\ s a -> s{_rbrRateKey = a});

-- | The maximum number of requests, which have an identical value in the field specified by the @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
rbrRateLimit :: Lens' RateBasedRule Natural
rbrRateLimit = lens _rbrRateLimit (\ s a -> s{_rbrRateLimit = a}) . _Nat;

instance FromJSON RateBasedRule where
        parseJSON
          = withObject "RateBasedRule"
              (\ x ->
                 RateBasedRule' <$>
                   (x .:? "MetricName") <*> (x .:? "Name") <*>
                     (x .: "RuleId")
                     <*> (x .:? "MatchPredicates" .!= mempty)
                     <*> (x .: "RateKey")
                     <*> (x .: "RateLimit"))

instance Hashable RateBasedRule

instance NFData RateBasedRule

-- | A combination of 'ByteMatchSet' , 'IPSet' , and/or 'SqlInjectionMatchSet' objects that identify the web requests that you want to allow, block, or count. For example, you might create a @Rule@ that includes the following predicates:
--
--
--     * An @IPSet@ that causes AWS WAF to search for web requests that originate from the IP address @192.0.2.44@
--
--     * A @ByteMatchSet@ that causes AWS WAF to search for web requests for which the value of the @User-Agent@ header is @BadBot@ .
--
--
--
-- To match the settings in this @Rule@ , a request must originate from @192.0.2.44@ AND include a @User-Agent@ header for which the value is @BadBot@ .
--
--
-- /See:/ 'rule' smart constructor.
data Rule = Rule'
    { _rMetricName :: !(Maybe Text)
    , _rName       :: !(Maybe Text)
    , _rRuleId     :: !Text
    , _rPredicates :: ![Predicate]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Rule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rMetricName' - A friendly name or description for the metrics for this @Rule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9); the name can't contain whitespace. You can't change @MetricName@ after you create the @Rule@ .
--
-- * 'rName' - The friendly name or description for the @Rule@ . You can't change the name of a @Rule@ after you create it.
--
-- * 'rRuleId' - A unique identifier for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete a one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ). @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
--
-- * 'rPredicates' - The @Predicates@ object contains one @Predicate@ element for each 'ByteMatchSet' , 'IPSet' , or 'SqlInjectionMatchSet' object that you want to include in a @Rule@ .
rule
    :: Text -- ^ 'rRuleId'
    -> Rule
rule pRuleId_ =
    Rule'
    { _rMetricName = Nothing
    , _rName = Nothing
    , _rRuleId = pRuleId_
    , _rPredicates = mempty
    }

-- | A friendly name or description for the metrics for this @Rule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9); the name can't contain whitespace. You can't change @MetricName@ after you create the @Rule@ .
rMetricName :: Lens' Rule (Maybe Text)
rMetricName = lens _rMetricName (\ s a -> s{_rMetricName = a});

-- | The friendly name or description for the @Rule@ . You can't change the name of a @Rule@ after you create it.
rName :: Lens' Rule (Maybe Text)
rName = lens _rName (\ s a -> s{_rName = a});

-- | A unique identifier for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete a one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ). @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
rRuleId :: Lens' Rule Text
rRuleId = lens _rRuleId (\ s a -> s{_rRuleId = a});

-- | The @Predicates@ object contains one @Predicate@ element for each 'ByteMatchSet' , 'IPSet' , or 'SqlInjectionMatchSet' object that you want to include in a @Rule@ .
rPredicates :: Lens' Rule [Predicate]
rPredicates = lens _rPredicates (\ s a -> s{_rPredicates = a}) . _Coerce;

instance FromJSON Rule where
        parseJSON
          = withObject "Rule"
              (\ x ->
                 Rule' <$>
                   (x .:? "MetricName") <*> (x .:? "Name") <*>
                     (x .: "RuleId")
                     <*> (x .:? "Predicates" .!= mempty))

instance Hashable Rule

instance NFData Rule

-- | Contains the identifier and the friendly name or description of the @Rule@ .
--
--
--
-- /See:/ 'ruleSummary' smart constructor.
data RuleSummary = RuleSummary'
    { _rsRuleId :: !Text
    , _rsName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RuleSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsRuleId' - A unique identifier for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ). @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
--
-- * 'rsName' - A friendly name or description of the 'Rule' . You can't change the name of a @Rule@ after you create it.
ruleSummary
    :: Text -- ^ 'rsRuleId'
    -> Text -- ^ 'rsName'
    -> RuleSummary
ruleSummary pRuleId_ pName_ =
    RuleSummary'
    { _rsRuleId = pRuleId_
    , _rsName = pName_
    }

-- | A unique identifier for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ). @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
rsRuleId :: Lens' RuleSummary Text
rsRuleId = lens _rsRuleId (\ s a -> s{_rsRuleId = a});

-- | A friendly name or description of the 'Rule' . You can't change the name of a @Rule@ after you create it.
rsName :: Lens' RuleSummary Text
rsName = lens _rsName (\ s a -> s{_rsName = a});

instance FromJSON RuleSummary where
        parseJSON
          = withObject "RuleSummary"
              (\ x ->
                 RuleSummary' <$> (x .: "RuleId") <*> (x .: "Name"))

instance Hashable RuleSummary

instance NFData RuleSummary

-- | Specifies a @Predicate@ (such as an @IPSet@ ) and indicates whether you want to add it to a @Rule@ or delete it from a @Rule@ .
--
--
--
-- /See:/ 'ruleUpdate' smart constructor.
data RuleUpdate = RuleUpdate'
    { _ruAction    :: !ChangeAction
    , _ruPredicate :: !Predicate
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RuleUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ruAction' - Specify @INSERT@ to add a @Predicate@ to a @Rule@ . Use @DELETE@ to remove a @Predicate@ from a @Rule@ .
--
-- * 'ruPredicate' - The ID of the @Predicate@ (such as an @IPSet@ ) that you want to add to a @Rule@ .
ruleUpdate
    :: ChangeAction -- ^ 'ruAction'
    -> Predicate -- ^ 'ruPredicate'
    -> RuleUpdate
ruleUpdate pAction_ pPredicate_ =
    RuleUpdate'
    { _ruAction = pAction_
    , _ruPredicate = pPredicate_
    }

-- | Specify @INSERT@ to add a @Predicate@ to a @Rule@ . Use @DELETE@ to remove a @Predicate@ from a @Rule@ .
ruAction :: Lens' RuleUpdate ChangeAction
ruAction = lens _ruAction (\ s a -> s{_ruAction = a});

-- | The ID of the @Predicate@ (such as an @IPSet@ ) that you want to add to a @Rule@ .
ruPredicate :: Lens' RuleUpdate Predicate
ruPredicate = lens _ruPredicate (\ s a -> s{_ruPredicate = a});

instance Hashable RuleUpdate

instance NFData RuleUpdate

instance ToJSON RuleUpdate where
        toJSON RuleUpdate'{..}
          = object
              (catMaybes
                 [Just ("Action" .= _ruAction),
                  Just ("Predicate" .= _ruPredicate)])

-- | The response from a 'GetSampledRequests' request includes a @SampledHTTPRequests@ complex type that appears as @SampledRequests@ in the response syntax. @SampledHTTPRequests@ contains one @SampledHTTPRequest@ object for each web request that is returned by @GetSampledRequests@ .
--
--
--
-- /See:/ 'sampledHTTPRequest' smart constructor.
data SampledHTTPRequest = SampledHTTPRequest'
    { _shttprAction    :: !(Maybe Text)
    , _shttprTimestamp :: !(Maybe POSIX)
    , _shttprRequest   :: !HTTPRequest
    , _shttprWeight    :: !Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SampledHTTPRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'shttprAction' - The action for the @Rule@ that the request matched: @ALLOW@ , @BLOCK@ , or @COUNT@ .
--
-- * 'shttprTimestamp' - The time at which AWS WAF received the request from your AWS resource, in Unix time format (in seconds).
--
-- * 'shttprRequest' - A complex type that contains detailed information about the request.
--
-- * 'shttprWeight' - A value that indicates how one result in the response relates proportionally to other results in the response. A result that has a weight of @2@ represents roughly twice as many CloudFront web requests as a result that has a weight of @1@ .
sampledHTTPRequest
    :: HTTPRequest -- ^ 'shttprRequest'
    -> Natural -- ^ 'shttprWeight'
    -> SampledHTTPRequest
sampledHTTPRequest pRequest_ pWeight_ =
    SampledHTTPRequest'
    { _shttprAction = Nothing
    , _shttprTimestamp = Nothing
    , _shttprRequest = pRequest_
    , _shttprWeight = _Nat # pWeight_
    }

-- | The action for the @Rule@ that the request matched: @ALLOW@ , @BLOCK@ , or @COUNT@ .
shttprAction :: Lens' SampledHTTPRequest (Maybe Text)
shttprAction = lens _shttprAction (\ s a -> s{_shttprAction = a});

-- | The time at which AWS WAF received the request from your AWS resource, in Unix time format (in seconds).
shttprTimestamp :: Lens' SampledHTTPRequest (Maybe UTCTime)
shttprTimestamp = lens _shttprTimestamp (\ s a -> s{_shttprTimestamp = a}) . mapping _Time;

-- | A complex type that contains detailed information about the request.
shttprRequest :: Lens' SampledHTTPRequest HTTPRequest
shttprRequest = lens _shttprRequest (\ s a -> s{_shttprRequest = a});

-- | A value that indicates how one result in the response relates proportionally to other results in the response. A result that has a weight of @2@ represents roughly twice as many CloudFront web requests as a result that has a weight of @1@ .
shttprWeight :: Lens' SampledHTTPRequest Natural
shttprWeight = lens _shttprWeight (\ s a -> s{_shttprWeight = a}) . _Nat;

instance FromJSON SampledHTTPRequest where
        parseJSON
          = withObject "SampledHTTPRequest"
              (\ x ->
                 SampledHTTPRequest' <$>
                   (x .:? "Action") <*> (x .:? "Timestamp") <*>
                     (x .: "Request")
                     <*> (x .: "Weight"))

instance Hashable SampledHTTPRequest

instance NFData SampledHTTPRequest

-- | Specifies a constraint on the size of a part of the web request. AWS WAF uses the @Size@ , @ComparisonOperator@ , and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
--
--
--
-- /See:/ 'sizeConstraint' smart constructor.
data SizeConstraint = SizeConstraint'
    { _scFieldToMatch       :: !FieldToMatch
    , _scTextTransformation :: !TextTransformation
    , _scComparisonOperator :: !ComparisonOperator
    , _scSize               :: !Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SizeConstraint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scFieldToMatch' - Specifies where in a web request to look for the size constraint.
--
-- * 'scTextTransformation' - Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @FieldToMatch@ before inspecting a request for a match. Note that if you choose @BODY@ for the value of @Type@ , you must choose @NONE@ for @TextTransformation@ because CloudFront forwards only the first 8192 bytes for inspection.  __NONE__  Specify @NONE@ if you don't want to perform any text transformations. __CMD_LINE__  When you're concerned that attackers are injecting an operating system command line command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:     * Delete the following characters: \ " ' ^     * Delete spaces before the following characters: / (     * Replace the following characters with a space: , ;     * Replace multiple spaces with one space     * Convert uppercase letters (A-Z) to lowercase (a-z) __COMPRESS_WHITE_SPACE__  Use this option to replace the following characters with a space character (decimal 32):     * \f, formfeed, decimal 12     * \t, tab, decimal 9     * \n, newline, decimal 10     * \r, carriage return, decimal 13     * \v, vertical tab, decimal 11     * non-breaking space, decimal 160 @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space. __HTML_ENTITY_DECODE__  Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:     * Replaces @(ampersand)quot;@ with @"@      * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160     * Replaces @(ampersand)lt;@ with a "less than" symbol     * Replaces @(ampersand)gt;@ with @>@      * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters __LOWERCASE__  Use this option to convert uppercase letters (A-Z) to lowercase (a-z). __URL_DECODE__  Use this option to decode a URL-encoded value.
--
-- * 'scComparisonOperator' - The type of comparison you want AWS WAF to perform. AWS WAF uses this in combination with the provided @Size@ and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match. __EQ__ : Used to test if the @Size@ is equal to the size of the @FieldToMatch@  __NE__ : Used to test if the @Size@ is not equal to the size of the @FieldToMatch@  __LE__ : Used to test if the @Size@ is less than or equal to the size of the @FieldToMatch@  __LT__ : Used to test if the @Size@ is strictly less than the size of the @FieldToMatch@  __GE__ : Used to test if the @Size@ is greater than or equal to the size of the @FieldToMatch@  __GT__ : Used to test if the @Size@ is strictly greater than the size of the @FieldToMatch@
--
-- * 'scSize' - The size in bytes that you want AWS WAF to compare against the size of the specified @FieldToMatch@ . AWS WAF uses this in combination with @ComparisonOperator@ and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match. Valid values for size are 0 - 21474836480 bytes (0 - 20 GB). If you specify @URI@ for the value of @Type@ , the / in the URI counts as one character. For example, the URI @/logo.jpg@ is nine characters long.
sizeConstraint
    :: FieldToMatch -- ^ 'scFieldToMatch'
    -> TextTransformation -- ^ 'scTextTransformation'
    -> ComparisonOperator -- ^ 'scComparisonOperator'
    -> Natural -- ^ 'scSize'
    -> SizeConstraint
sizeConstraint pFieldToMatch_ pTextTransformation_ pComparisonOperator_ pSize_ =
    SizeConstraint'
    { _scFieldToMatch = pFieldToMatch_
    , _scTextTransformation = pTextTransformation_
    , _scComparisonOperator = pComparisonOperator_
    , _scSize = _Nat # pSize_
    }

-- | Specifies where in a web request to look for the size constraint.
scFieldToMatch :: Lens' SizeConstraint FieldToMatch
scFieldToMatch = lens _scFieldToMatch (\ s a -> s{_scFieldToMatch = a});

-- | Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @FieldToMatch@ before inspecting a request for a match. Note that if you choose @BODY@ for the value of @Type@ , you must choose @NONE@ for @TextTransformation@ because CloudFront forwards only the first 8192 bytes for inspection.  __NONE__  Specify @NONE@ if you don't want to perform any text transformations. __CMD_LINE__  When you're concerned that attackers are injecting an operating system command line command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:     * Delete the following characters: \ " ' ^     * Delete spaces before the following characters: / (     * Replace the following characters with a space: , ;     * Replace multiple spaces with one space     * Convert uppercase letters (A-Z) to lowercase (a-z) __COMPRESS_WHITE_SPACE__  Use this option to replace the following characters with a space character (decimal 32):     * \f, formfeed, decimal 12     * \t, tab, decimal 9     * \n, newline, decimal 10     * \r, carriage return, decimal 13     * \v, vertical tab, decimal 11     * non-breaking space, decimal 160 @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space. __HTML_ENTITY_DECODE__  Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:     * Replaces @(ampersand)quot;@ with @"@      * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160     * Replaces @(ampersand)lt;@ with a "less than" symbol     * Replaces @(ampersand)gt;@ with @>@      * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters __LOWERCASE__  Use this option to convert uppercase letters (A-Z) to lowercase (a-z). __URL_DECODE__  Use this option to decode a URL-encoded value.
scTextTransformation :: Lens' SizeConstraint TextTransformation
scTextTransformation = lens _scTextTransformation (\ s a -> s{_scTextTransformation = a});

-- | The type of comparison you want AWS WAF to perform. AWS WAF uses this in combination with the provided @Size@ and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match. __EQ__ : Used to test if the @Size@ is equal to the size of the @FieldToMatch@  __NE__ : Used to test if the @Size@ is not equal to the size of the @FieldToMatch@  __LE__ : Used to test if the @Size@ is less than or equal to the size of the @FieldToMatch@  __LT__ : Used to test if the @Size@ is strictly less than the size of the @FieldToMatch@  __GE__ : Used to test if the @Size@ is greater than or equal to the size of the @FieldToMatch@  __GT__ : Used to test if the @Size@ is strictly greater than the size of the @FieldToMatch@
scComparisonOperator :: Lens' SizeConstraint ComparisonOperator
scComparisonOperator = lens _scComparisonOperator (\ s a -> s{_scComparisonOperator = a});

-- | The size in bytes that you want AWS WAF to compare against the size of the specified @FieldToMatch@ . AWS WAF uses this in combination with @ComparisonOperator@ and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match. Valid values for size are 0 - 21474836480 bytes (0 - 20 GB). If you specify @URI@ for the value of @Type@ , the / in the URI counts as one character. For example, the URI @/logo.jpg@ is nine characters long.
scSize :: Lens' SizeConstraint Natural
scSize = lens _scSize (\ s a -> s{_scSize = a}) . _Nat;

instance FromJSON SizeConstraint where
        parseJSON
          = withObject "SizeConstraint"
              (\ x ->
                 SizeConstraint' <$>
                   (x .: "FieldToMatch") <*> (x .: "TextTransformation")
                     <*> (x .: "ComparisonOperator")
                     <*> (x .: "Size"))

instance Hashable SizeConstraint

instance NFData SizeConstraint

instance ToJSON SizeConstraint where
        toJSON SizeConstraint'{..}
          = object
              (catMaybes
                 [Just ("FieldToMatch" .= _scFieldToMatch),
                  Just ("TextTransformation" .= _scTextTransformation),
                  Just ("ComparisonOperator" .= _scComparisonOperator),
                  Just ("Size" .= _scSize)])

-- | A complex type that contains @SizeConstraint@ objects, which specify the parts of web requests that you want AWS WAF to inspect the size of. If a @SizeConstraintSet@ contains more than one @SizeConstraint@ object, a request only needs to match one constraint to be considered a match.
--
--
--
-- /See:/ 'sizeConstraintSet' smart constructor.
data SizeConstraintSet = SizeConstraintSet'
    { _scsName                :: !(Maybe Text)
    , _scsSizeConstraintSetId :: !Text
    , _scsSizeConstraints     :: ![SizeConstraint]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SizeConstraintSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scsName' - The name, if any, of the @SizeConstraintSet@ .
--
-- * 'scsSizeConstraintSetId' - A unique identifier for a @SizeConstraintSet@ . You use @SizeConstraintSetId@ to get information about a @SizeConstraintSet@ (see 'GetSizeConstraintSet' ), update a @SizeConstraintSet@ (see 'UpdateSizeConstraintSet' ), insert a @SizeConstraintSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SizeConstraintSet@ from AWS WAF (see 'DeleteSizeConstraintSet' ). @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
--
-- * 'scsSizeConstraints' - Specifies the parts of web requests that you want to inspect the size of.
sizeConstraintSet
    :: Text -- ^ 'scsSizeConstraintSetId'
    -> SizeConstraintSet
sizeConstraintSet pSizeConstraintSetId_ =
    SizeConstraintSet'
    { _scsName = Nothing
    , _scsSizeConstraintSetId = pSizeConstraintSetId_
    , _scsSizeConstraints = mempty
    }

-- | The name, if any, of the @SizeConstraintSet@ .
scsName :: Lens' SizeConstraintSet (Maybe Text)
scsName = lens _scsName (\ s a -> s{_scsName = a});

-- | A unique identifier for a @SizeConstraintSet@ . You use @SizeConstraintSetId@ to get information about a @SizeConstraintSet@ (see 'GetSizeConstraintSet' ), update a @SizeConstraintSet@ (see 'UpdateSizeConstraintSet' ), insert a @SizeConstraintSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SizeConstraintSet@ from AWS WAF (see 'DeleteSizeConstraintSet' ). @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
scsSizeConstraintSetId :: Lens' SizeConstraintSet Text
scsSizeConstraintSetId = lens _scsSizeConstraintSetId (\ s a -> s{_scsSizeConstraintSetId = a});

-- | Specifies the parts of web requests that you want to inspect the size of.
scsSizeConstraints :: Lens' SizeConstraintSet [SizeConstraint]
scsSizeConstraints = lens _scsSizeConstraints (\ s a -> s{_scsSizeConstraints = a}) . _Coerce;

instance FromJSON SizeConstraintSet where
        parseJSON
          = withObject "SizeConstraintSet"
              (\ x ->
                 SizeConstraintSet' <$>
                   (x .:? "Name") <*> (x .: "SizeConstraintSetId") <*>
                     (x .:? "SizeConstraints" .!= mempty))

instance Hashable SizeConstraintSet

instance NFData SizeConstraintSet

-- | The @Id@ and @Name@ of a @SizeConstraintSet@ .
--
--
--
-- /See:/ 'sizeConstraintSetSummary' smart constructor.
data SizeConstraintSetSummary = SizeConstraintSetSummary'
    { _scssSizeConstraintSetId :: !Text
    , _scssName                :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SizeConstraintSetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scssSizeConstraintSetId' - A unique identifier for a @SizeConstraintSet@ . You use @SizeConstraintSetId@ to get information about a @SizeConstraintSet@ (see 'GetSizeConstraintSet' ), update a @SizeConstraintSet@ (see 'UpdateSizeConstraintSet' ), insert a @SizeConstraintSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SizeConstraintSet@ from AWS WAF (see 'DeleteSizeConstraintSet' ). @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
--
-- * 'scssName' - The name of the @SizeConstraintSet@ , if any.
sizeConstraintSetSummary
    :: Text -- ^ 'scssSizeConstraintSetId'
    -> Text -- ^ 'scssName'
    -> SizeConstraintSetSummary
sizeConstraintSetSummary pSizeConstraintSetId_ pName_ =
    SizeConstraintSetSummary'
    { _scssSizeConstraintSetId = pSizeConstraintSetId_
    , _scssName = pName_
    }

-- | A unique identifier for a @SizeConstraintSet@ . You use @SizeConstraintSetId@ to get information about a @SizeConstraintSet@ (see 'GetSizeConstraintSet' ), update a @SizeConstraintSet@ (see 'UpdateSizeConstraintSet' ), insert a @SizeConstraintSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SizeConstraintSet@ from AWS WAF (see 'DeleteSizeConstraintSet' ). @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
scssSizeConstraintSetId :: Lens' SizeConstraintSetSummary Text
scssSizeConstraintSetId = lens _scssSizeConstraintSetId (\ s a -> s{_scssSizeConstraintSetId = a});

-- | The name of the @SizeConstraintSet@ , if any.
scssName :: Lens' SizeConstraintSetSummary Text
scssName = lens _scssName (\ s a -> s{_scssName = a});

instance FromJSON SizeConstraintSetSummary where
        parseJSON
          = withObject "SizeConstraintSetSummary"
              (\ x ->
                 SizeConstraintSetSummary' <$>
                   (x .: "SizeConstraintSetId") <*> (x .: "Name"))

instance Hashable SizeConstraintSetSummary

instance NFData SizeConstraintSetSummary

-- | Specifies the part of a web request that you want to inspect the size of and indicates whether you want to add the specification to a 'SizeConstraintSet' or delete it from a @SizeConstraintSet@ .
--
--
--
-- /See:/ 'sizeConstraintSetUpdate' smart constructor.
data SizeConstraintSetUpdate = SizeConstraintSetUpdate'
    { _scsuAction         :: !ChangeAction
    , _scsuSizeConstraint :: !SizeConstraint
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SizeConstraintSetUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scsuAction' - Specify @INSERT@ to add a 'SizeConstraintSetUpdate' to a 'SizeConstraintSet' . Use @DELETE@ to remove a @SizeConstraintSetUpdate@ from a @SizeConstraintSet@ .
--
-- * 'scsuSizeConstraint' - Specifies a constraint on the size of a part of the web request. AWS WAF uses the @Size@ , @ComparisonOperator@ , and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
sizeConstraintSetUpdate
    :: ChangeAction -- ^ 'scsuAction'
    -> SizeConstraint -- ^ 'scsuSizeConstraint'
    -> SizeConstraintSetUpdate
sizeConstraintSetUpdate pAction_ pSizeConstraint_ =
    SizeConstraintSetUpdate'
    { _scsuAction = pAction_
    , _scsuSizeConstraint = pSizeConstraint_
    }

-- | Specify @INSERT@ to add a 'SizeConstraintSetUpdate' to a 'SizeConstraintSet' . Use @DELETE@ to remove a @SizeConstraintSetUpdate@ from a @SizeConstraintSet@ .
scsuAction :: Lens' SizeConstraintSetUpdate ChangeAction
scsuAction = lens _scsuAction (\ s a -> s{_scsuAction = a});

-- | Specifies a constraint on the size of a part of the web request. AWS WAF uses the @Size@ , @ComparisonOperator@ , and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
scsuSizeConstraint :: Lens' SizeConstraintSetUpdate SizeConstraint
scsuSizeConstraint = lens _scsuSizeConstraint (\ s a -> s{_scsuSizeConstraint = a});

instance Hashable SizeConstraintSetUpdate

instance NFData SizeConstraintSetUpdate

instance ToJSON SizeConstraintSetUpdate where
        toJSON SizeConstraintSetUpdate'{..}
          = object
              (catMaybes
                 [Just ("Action" .= _scsuAction),
                  Just ("SizeConstraint" .= _scsuSizeConstraint)])

-- | A complex type that contains @SqlInjectionMatchTuple@ objects, which specify the parts of web requests that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header. If a @SqlInjectionMatchSet@ contains more than one @SqlInjectionMatchTuple@ object, a request needs to include snippets of SQL code in only one of the specified parts of the request to be considered a match.
--
--
--
-- /See:/ 'sqlInjectionMatchSet' smart constructor.
data SqlInjectionMatchSet = SqlInjectionMatchSet'
    { _simsName                    :: !(Maybe Text)
    , _simsSqlInjectionMatchSetId  :: !Text
    , _simsSqlInjectionMatchTuples :: ![SqlInjectionMatchTuple]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SqlInjectionMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'simsName' - The name, if any, of the @SqlInjectionMatchSet@ .
--
-- * 'simsSqlInjectionMatchSetId' - A unique identifier for a @SqlInjectionMatchSet@ . You use @SqlInjectionMatchSetId@ to get information about a @SqlInjectionMatchSet@ (see 'GetSqlInjectionMatchSet' ), update a @SqlInjectionMatchSet@ (see 'UpdateSqlInjectionMatchSet' ), insert a @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SqlInjectionMatchSet@ from AWS WAF (see 'DeleteSqlInjectionMatchSet' ). @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
--
-- * 'simsSqlInjectionMatchTuples' - Specifies the parts of web requests that you want to inspect for snippets of malicious SQL code.
sqlInjectionMatchSet
    :: Text -- ^ 'simsSqlInjectionMatchSetId'
    -> SqlInjectionMatchSet
sqlInjectionMatchSet pSqlInjectionMatchSetId_ =
    SqlInjectionMatchSet'
    { _simsName = Nothing
    , _simsSqlInjectionMatchSetId = pSqlInjectionMatchSetId_
    , _simsSqlInjectionMatchTuples = mempty
    }

-- | The name, if any, of the @SqlInjectionMatchSet@ .
simsName :: Lens' SqlInjectionMatchSet (Maybe Text)
simsName = lens _simsName (\ s a -> s{_simsName = a});

-- | A unique identifier for a @SqlInjectionMatchSet@ . You use @SqlInjectionMatchSetId@ to get information about a @SqlInjectionMatchSet@ (see 'GetSqlInjectionMatchSet' ), update a @SqlInjectionMatchSet@ (see 'UpdateSqlInjectionMatchSet' ), insert a @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SqlInjectionMatchSet@ from AWS WAF (see 'DeleteSqlInjectionMatchSet' ). @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
simsSqlInjectionMatchSetId :: Lens' SqlInjectionMatchSet Text
simsSqlInjectionMatchSetId = lens _simsSqlInjectionMatchSetId (\ s a -> s{_simsSqlInjectionMatchSetId = a});

-- | Specifies the parts of web requests that you want to inspect for snippets of malicious SQL code.
simsSqlInjectionMatchTuples :: Lens' SqlInjectionMatchSet [SqlInjectionMatchTuple]
simsSqlInjectionMatchTuples = lens _simsSqlInjectionMatchTuples (\ s a -> s{_simsSqlInjectionMatchTuples = a}) . _Coerce;

instance FromJSON SqlInjectionMatchSet where
        parseJSON
          = withObject "SqlInjectionMatchSet"
              (\ x ->
                 SqlInjectionMatchSet' <$>
                   (x .:? "Name") <*> (x .: "SqlInjectionMatchSetId")
                     <*> (x .:? "SqlInjectionMatchTuples" .!= mempty))

instance Hashable SqlInjectionMatchSet

instance NFData SqlInjectionMatchSet

-- | The @Id@ and @Name@ of a @SqlInjectionMatchSet@ .
--
--
--
-- /See:/ 'sqlInjectionMatchSetSummary' smart constructor.
data SqlInjectionMatchSetSummary = SqlInjectionMatchSetSummary'
    { _simssSqlInjectionMatchSetId :: !Text
    , _simssName                   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SqlInjectionMatchSetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'simssSqlInjectionMatchSetId' - A unique identifier for a @SqlInjectionMatchSet@ . You use @SqlInjectionMatchSetId@ to get information about a @SqlInjectionMatchSet@ (see 'GetSqlInjectionMatchSet' ), update a @SqlInjectionMatchSet@ (see 'UpdateSqlInjectionMatchSet' ), insert a @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SqlInjectionMatchSet@ from AWS WAF (see 'DeleteSqlInjectionMatchSet' ). @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
--
-- * 'simssName' - The name of the @SqlInjectionMatchSet@ , if any, specified by @Id@ .
sqlInjectionMatchSetSummary
    :: Text -- ^ 'simssSqlInjectionMatchSetId'
    -> Text -- ^ 'simssName'
    -> SqlInjectionMatchSetSummary
sqlInjectionMatchSetSummary pSqlInjectionMatchSetId_ pName_ =
    SqlInjectionMatchSetSummary'
    { _simssSqlInjectionMatchSetId = pSqlInjectionMatchSetId_
    , _simssName = pName_
    }

-- | A unique identifier for a @SqlInjectionMatchSet@ . You use @SqlInjectionMatchSetId@ to get information about a @SqlInjectionMatchSet@ (see 'GetSqlInjectionMatchSet' ), update a @SqlInjectionMatchSet@ (see 'UpdateSqlInjectionMatchSet' ), insert a @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SqlInjectionMatchSet@ from AWS WAF (see 'DeleteSqlInjectionMatchSet' ). @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
simssSqlInjectionMatchSetId :: Lens' SqlInjectionMatchSetSummary Text
simssSqlInjectionMatchSetId = lens _simssSqlInjectionMatchSetId (\ s a -> s{_simssSqlInjectionMatchSetId = a});

-- | The name of the @SqlInjectionMatchSet@ , if any, specified by @Id@ .
simssName :: Lens' SqlInjectionMatchSetSummary Text
simssName = lens _simssName (\ s a -> s{_simssName = a});

instance FromJSON SqlInjectionMatchSetSummary where
        parseJSON
          = withObject "SqlInjectionMatchSetSummary"
              (\ x ->
                 SqlInjectionMatchSetSummary' <$>
                   (x .: "SqlInjectionMatchSetId") <*> (x .: "Name"))

instance Hashable SqlInjectionMatchSetSummary

instance NFData SqlInjectionMatchSetSummary

-- | Specifies the part of a web request that you want to inspect for snippets of malicious SQL code and indicates whether you want to add the specification to a 'SqlInjectionMatchSet' or delete it from a @SqlInjectionMatchSet@ .
--
--
--
-- /See:/ 'sqlInjectionMatchSetUpdate' smart constructor.
data SqlInjectionMatchSetUpdate = SqlInjectionMatchSetUpdate'
    { _simsuAction                 :: !ChangeAction
    , _simsuSqlInjectionMatchTuple :: !SqlInjectionMatchTuple
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SqlInjectionMatchSetUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'simsuAction' - Specify @INSERT@ to add a 'SqlInjectionMatchSetUpdate' to a 'SqlInjectionMatchSet' . Use @DELETE@ to remove a @SqlInjectionMatchSetUpdate@ from a @SqlInjectionMatchSet@ .
--
-- * 'simsuSqlInjectionMatchTuple' - Specifies the part of a web request that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header.
sqlInjectionMatchSetUpdate
    :: ChangeAction -- ^ 'simsuAction'
    -> SqlInjectionMatchTuple -- ^ 'simsuSqlInjectionMatchTuple'
    -> SqlInjectionMatchSetUpdate
sqlInjectionMatchSetUpdate pAction_ pSqlInjectionMatchTuple_ =
    SqlInjectionMatchSetUpdate'
    { _simsuAction = pAction_
    , _simsuSqlInjectionMatchTuple = pSqlInjectionMatchTuple_
    }

-- | Specify @INSERT@ to add a 'SqlInjectionMatchSetUpdate' to a 'SqlInjectionMatchSet' . Use @DELETE@ to remove a @SqlInjectionMatchSetUpdate@ from a @SqlInjectionMatchSet@ .
simsuAction :: Lens' SqlInjectionMatchSetUpdate ChangeAction
simsuAction = lens _simsuAction (\ s a -> s{_simsuAction = a});

-- | Specifies the part of a web request that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header.
simsuSqlInjectionMatchTuple :: Lens' SqlInjectionMatchSetUpdate SqlInjectionMatchTuple
simsuSqlInjectionMatchTuple = lens _simsuSqlInjectionMatchTuple (\ s a -> s{_simsuSqlInjectionMatchTuple = a});

instance Hashable SqlInjectionMatchSetUpdate

instance NFData SqlInjectionMatchSetUpdate

instance ToJSON SqlInjectionMatchSetUpdate where
        toJSON SqlInjectionMatchSetUpdate'{..}
          = object
              (catMaybes
                 [Just ("Action" .= _simsuAction),
                  Just
                    ("SqlInjectionMatchTuple" .=
                       _simsuSqlInjectionMatchTuple)])

-- | Specifies the part of a web request that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header.
--
--
--
-- /See:/ 'sqlInjectionMatchTuple' smart constructor.
data SqlInjectionMatchTuple = SqlInjectionMatchTuple'
    { _simtFieldToMatch       :: !FieldToMatch
    , _simtTextTransformation :: !TextTransformation
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SqlInjectionMatchTuple' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'simtFieldToMatch' - Specifies where in a web request to look for snippets of malicious SQL code.
--
-- * 'simtTextTransformation' - Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @FieldToMatch@ before inspecting a request for a match. __CMD_LINE__  When you're concerned that attackers are injecting an operating system commandline command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:     * Delete the following characters: \ " ' ^     * Delete spaces before the following characters: / (     * Replace the following characters with a space: , ;     * Replace multiple spaces with one space     * Convert uppercase letters (A-Z) to lowercase (a-z) __COMPRESS_WHITE_SPACE__  Use this option to replace the following characters with a space character (decimal 32):     * \f, formfeed, decimal 12     * \t, tab, decimal 9     * \n, newline, decimal 10     * \r, carriage return, decimal 13     * \v, vertical tab, decimal 11     * non-breaking space, decimal 160 @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space. __HTML_ENTITY_DECODE__  Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:     * Replaces @(ampersand)quot;@ with @"@      * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160     * Replaces @(ampersand)lt;@ with a "less than" symbol     * Replaces @(ampersand)gt;@ with @>@      * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters __LOWERCASE__  Use this option to convert uppercase letters (A-Z) to lowercase (a-z). __URL_DECODE__  Use this option to decode a URL-encoded value. __NONE__  Specify @NONE@ if you don't want to perform any text transformations.
sqlInjectionMatchTuple
    :: FieldToMatch -- ^ 'simtFieldToMatch'
    -> TextTransformation -- ^ 'simtTextTransformation'
    -> SqlInjectionMatchTuple
sqlInjectionMatchTuple pFieldToMatch_ pTextTransformation_ =
    SqlInjectionMatchTuple'
    { _simtFieldToMatch = pFieldToMatch_
    , _simtTextTransformation = pTextTransformation_
    }

-- | Specifies where in a web request to look for snippets of malicious SQL code.
simtFieldToMatch :: Lens' SqlInjectionMatchTuple FieldToMatch
simtFieldToMatch = lens _simtFieldToMatch (\ s a -> s{_simtFieldToMatch = a});

-- | Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @FieldToMatch@ before inspecting a request for a match. __CMD_LINE__  When you're concerned that attackers are injecting an operating system commandline command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:     * Delete the following characters: \ " ' ^     * Delete spaces before the following characters: / (     * Replace the following characters with a space: , ;     * Replace multiple spaces with one space     * Convert uppercase letters (A-Z) to lowercase (a-z) __COMPRESS_WHITE_SPACE__  Use this option to replace the following characters with a space character (decimal 32):     * \f, formfeed, decimal 12     * \t, tab, decimal 9     * \n, newline, decimal 10     * \r, carriage return, decimal 13     * \v, vertical tab, decimal 11     * non-breaking space, decimal 160 @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space. __HTML_ENTITY_DECODE__  Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:     * Replaces @(ampersand)quot;@ with @"@      * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160     * Replaces @(ampersand)lt;@ with a "less than" symbol     * Replaces @(ampersand)gt;@ with @>@      * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters __LOWERCASE__  Use this option to convert uppercase letters (A-Z) to lowercase (a-z). __URL_DECODE__  Use this option to decode a URL-encoded value. __NONE__  Specify @NONE@ if you don't want to perform any text transformations.
simtTextTransformation :: Lens' SqlInjectionMatchTuple TextTransformation
simtTextTransformation = lens _simtTextTransformation (\ s a -> s{_simtTextTransformation = a});

instance FromJSON SqlInjectionMatchTuple where
        parseJSON
          = withObject "SqlInjectionMatchTuple"
              (\ x ->
                 SqlInjectionMatchTuple' <$>
                   (x .: "FieldToMatch") <*>
                     (x .: "TextTransformation"))

instance Hashable SqlInjectionMatchTuple

instance NFData SqlInjectionMatchTuple

instance ToJSON SqlInjectionMatchTuple where
        toJSON SqlInjectionMatchTuple'{..}
          = object
              (catMaybes
                 [Just ("FieldToMatch" .= _simtFieldToMatch),
                  Just
                    ("TextTransformation" .= _simtTextTransformation)])

-- | In a 'GetSampledRequests' request, the @StartTime@ and @EndTime@ objects specify the time range for which you want AWS WAF to return a sample of web requests.
--
--
-- In a 'GetSampledRequests' response, the @StartTime@ and @EndTime@ objects specify the time range for which AWS WAF actually returned a sample of web requests. AWS WAF gets the specified number of requests from among the first 5,000 requests that your AWS resource receives during the specified time period. If your resource receives more than 5,000 requests during that period, AWS WAF stops sampling after the 5,000th request. In that case, @EndTime@ is the time that AWS WAF received the 5,000th request.
--
--
-- /See:/ 'timeWindow' smart constructor.
data TimeWindow = TimeWindow'
    { _twStartTime :: !POSIX
    , _twEndTime   :: !POSIX
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TimeWindow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'twStartTime' - The beginning of the time range from which you want @GetSampledRequests@ to return a sample of the requests that your AWS resource received. Specify the date and time in the following format: @"2016-09-27T14:50Z"@ . You can specify any time range in the previous three hours.
--
-- * 'twEndTime' - The end of the time range from which you want @GetSampledRequests@ to return a sample of the requests that your AWS resource received. Specify the date and time in the following format: @"2016-09-27T14:50Z"@ . You can specify any time range in the previous three hours.
timeWindow
    :: UTCTime -- ^ 'twStartTime'
    -> UTCTime -- ^ 'twEndTime'
    -> TimeWindow
timeWindow pStartTime_ pEndTime_ =
    TimeWindow'
    { _twStartTime = _Time # pStartTime_
    , _twEndTime = _Time # pEndTime_
    }

-- | The beginning of the time range from which you want @GetSampledRequests@ to return a sample of the requests that your AWS resource received. Specify the date and time in the following format: @"2016-09-27T14:50Z"@ . You can specify any time range in the previous three hours.
twStartTime :: Lens' TimeWindow UTCTime
twStartTime = lens _twStartTime (\ s a -> s{_twStartTime = a}) . _Time;

-- | The end of the time range from which you want @GetSampledRequests@ to return a sample of the requests that your AWS resource received. Specify the date and time in the following format: @"2016-09-27T14:50Z"@ . You can specify any time range in the previous three hours.
twEndTime :: Lens' TimeWindow UTCTime
twEndTime = lens _twEndTime (\ s a -> s{_twEndTime = a}) . _Time;

instance FromJSON TimeWindow where
        parseJSON
          = withObject "TimeWindow"
              (\ x ->
                 TimeWindow' <$>
                   (x .: "StartTime") <*> (x .: "EndTime"))

instance Hashable TimeWindow

instance NFData TimeWindow

instance ToJSON TimeWindow where
        toJSON TimeWindow'{..}
          = object
              (catMaybes
                 [Just ("StartTime" .= _twStartTime),
                  Just ("EndTime" .= _twEndTime)])

-- | For the action that is associated with a rule in a @WebACL@ , specifies the action that you want AWS WAF to perform when a web request matches all of the conditions in a rule. For the default action in a @WebACL@ , specifies the action that you want AWS WAF to take when a web request doesn't match all of the conditions in any of the rules in a @WebACL@ .
--
--
--
-- /See:/ 'wafAction' smart constructor.
newtype WafAction = WafAction'
    { _waType :: WafActionType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'WafAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'waType' - Specifies how you want AWS WAF to respond to requests that match the settings in a @Rule@ . Valid settings include the following:     * @ALLOW@ : AWS WAF allows requests     * @BLOCK@ : AWS WAF blocks requests     * @COUNT@ : AWS WAF increments a counter of the requests that match all of the conditions in the rule. AWS WAF then continues to inspect the web request based on the remaining rules in the web ACL. You can't specify @COUNT@ for the default action for a @WebACL@ .
wafAction
    :: WafActionType -- ^ 'waType'
    -> WafAction
wafAction pType_ =
    WafAction'
    { _waType = pType_
    }

-- | Specifies how you want AWS WAF to respond to requests that match the settings in a @Rule@ . Valid settings include the following:     * @ALLOW@ : AWS WAF allows requests     * @BLOCK@ : AWS WAF blocks requests     * @COUNT@ : AWS WAF increments a counter of the requests that match all of the conditions in the rule. AWS WAF then continues to inspect the web request based on the remaining rules in the web ACL. You can't specify @COUNT@ for the default action for a @WebACL@ .
waType :: Lens' WafAction WafActionType
waType = lens _waType (\ s a -> s{_waType = a});

instance FromJSON WafAction where
        parseJSON
          = withObject "WafAction"
              (\ x -> WafAction' <$> (x .: "Type"))

instance Hashable WafAction

instance NFData WafAction

instance ToJSON WafAction where
        toJSON WafAction'{..}
          = object (catMaybes [Just ("Type" .= _waType)])

-- | Contains the @Rules@ that identify the requests that you want to allow, block, or count. In a @WebACL@ , you also specify a default action (@ALLOW@ or @BLOCK@ ), and the action for each @Rule@ that you add to a @WebACL@ , for example, block requests from specified IP addresses or block requests from specified referrers. You also associate the @WebACL@ with a CloudFront distribution to identify the requests that you want AWS WAF to filter. If you add more than one @Rule@ to a @WebACL@ , a request needs to match only one of the specifications to be allowed, blocked, or counted. For more information, see 'UpdateWebACL' .
--
--
--
-- /See:/ 'webACL' smart constructor.
data WebACL = WebACL'
    { _waMetricName    :: !(Maybe Text)
    , _waName          :: !(Maybe Text)
    , _waWebACLId      :: !Text
    , _waDefaultAction :: !WafAction
    , _waRules         :: ![ActivatedRule]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'WebACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'waMetricName' - A friendly name or description for the metrics for this @WebACL@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9); the name can't contain whitespace. You can't change @MetricName@ after you create the @WebACL@ .
--
-- * 'waName' - A friendly name or description of the @WebACL@ . You can't change the name of a @WebACL@ after you create it.
--
-- * 'waWebACLId' - A unique identifier for a @WebACL@ . You use @WebACLId@ to get information about a @WebACL@ (see 'GetWebACL' ), update a @WebACL@ (see 'UpdateWebACL' ), and delete a @WebACL@ from AWS WAF (see 'DeleteWebACL' ). @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
--
-- * 'waDefaultAction' - The action to perform if none of the @Rules@ contained in the @WebACL@ match. The action is specified by the 'WafAction' object.
--
-- * 'waRules' - An array that contains the action for each @Rule@ in a @WebACL@ , the priority of the @Rule@ , and the ID of the @Rule@ .
webACL
    :: Text -- ^ 'waWebACLId'
    -> WafAction -- ^ 'waDefaultAction'
    -> WebACL
webACL pWebACLId_ pDefaultAction_ =
    WebACL'
    { _waMetricName = Nothing
    , _waName = Nothing
    , _waWebACLId = pWebACLId_
    , _waDefaultAction = pDefaultAction_
    , _waRules = mempty
    }

-- | A friendly name or description for the metrics for this @WebACL@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9); the name can't contain whitespace. You can't change @MetricName@ after you create the @WebACL@ .
waMetricName :: Lens' WebACL (Maybe Text)
waMetricName = lens _waMetricName (\ s a -> s{_waMetricName = a});

-- | A friendly name or description of the @WebACL@ . You can't change the name of a @WebACL@ after you create it.
waName :: Lens' WebACL (Maybe Text)
waName = lens _waName (\ s a -> s{_waName = a});

-- | A unique identifier for a @WebACL@ . You use @WebACLId@ to get information about a @WebACL@ (see 'GetWebACL' ), update a @WebACL@ (see 'UpdateWebACL' ), and delete a @WebACL@ from AWS WAF (see 'DeleteWebACL' ). @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
waWebACLId :: Lens' WebACL Text
waWebACLId = lens _waWebACLId (\ s a -> s{_waWebACLId = a});

-- | The action to perform if none of the @Rules@ contained in the @WebACL@ match. The action is specified by the 'WafAction' object.
waDefaultAction :: Lens' WebACL WafAction
waDefaultAction = lens _waDefaultAction (\ s a -> s{_waDefaultAction = a});

-- | An array that contains the action for each @Rule@ in a @WebACL@ , the priority of the @Rule@ , and the ID of the @Rule@ .
waRules :: Lens' WebACL [ActivatedRule]
waRules = lens _waRules (\ s a -> s{_waRules = a}) . _Coerce;

instance FromJSON WebACL where
        parseJSON
          = withObject "WebACL"
              (\ x ->
                 WebACL' <$>
                   (x .:? "MetricName") <*> (x .:? "Name") <*>
                     (x .: "WebACLId")
                     <*> (x .: "DefaultAction")
                     <*> (x .:? "Rules" .!= mempty))

instance Hashable WebACL

instance NFData WebACL

-- | Contains the identifier and the name or description of the 'WebACL' .
--
--
--
-- /See:/ 'webACLSummary' smart constructor.
data WebACLSummary = WebACLSummary'
    { _wasWebACLId :: !Text
    , _wasName     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'WebACLSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wasWebACLId' - A unique identifier for a @WebACL@ . You use @WebACLId@ to get information about a @WebACL@ (see 'GetWebACL' ), update a @WebACL@ (see 'UpdateWebACL' ), and delete a @WebACL@ from AWS WAF (see 'DeleteWebACL' ). @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
--
-- * 'wasName' - A friendly name or description of the 'WebACL' . You can't change the name of a @WebACL@ after you create it.
webACLSummary
    :: Text -- ^ 'wasWebACLId'
    -> Text -- ^ 'wasName'
    -> WebACLSummary
webACLSummary pWebACLId_ pName_ =
    WebACLSummary'
    { _wasWebACLId = pWebACLId_
    , _wasName = pName_
    }

-- | A unique identifier for a @WebACL@ . You use @WebACLId@ to get information about a @WebACL@ (see 'GetWebACL' ), update a @WebACL@ (see 'UpdateWebACL' ), and delete a @WebACL@ from AWS WAF (see 'DeleteWebACL' ). @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
wasWebACLId :: Lens' WebACLSummary Text
wasWebACLId = lens _wasWebACLId (\ s a -> s{_wasWebACLId = a});

-- | A friendly name or description of the 'WebACL' . You can't change the name of a @WebACL@ after you create it.
wasName :: Lens' WebACLSummary Text
wasName = lens _wasName (\ s a -> s{_wasName = a});

instance FromJSON WebACLSummary where
        parseJSON
          = withObject "WebACLSummary"
              (\ x ->
                 WebACLSummary' <$>
                   (x .: "WebACLId") <*> (x .: "Name"))

instance Hashable WebACLSummary

instance NFData WebACLSummary

-- | Specifies whether to insert a @Rule@ into or delete a @Rule@ from a @WebACL@ .
--
--
--
-- /See:/ 'webACLUpdate' smart constructor.
data WebACLUpdate = WebACLUpdate'
    { _wauAction        :: !ChangeAction
    , _wauActivatedRule :: !ActivatedRule
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'WebACLUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wauAction' - Specifies whether to insert a @Rule@ into or delete a @Rule@ from a @WebACL@ .
--
-- * 'wauActivatedRule' - The @ActivatedRule@ object in an 'UpdateWebACL' request specifies a @Rule@ that you want to insert or delete, the priority of the @Rule@ in the @WebACL@ , and the action that you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@ , @BLOCK@ , or @COUNT@ ).
webACLUpdate
    :: ChangeAction -- ^ 'wauAction'
    -> ActivatedRule -- ^ 'wauActivatedRule'
    -> WebACLUpdate
webACLUpdate pAction_ pActivatedRule_ =
    WebACLUpdate'
    { _wauAction = pAction_
    , _wauActivatedRule = pActivatedRule_
    }

-- | Specifies whether to insert a @Rule@ into or delete a @Rule@ from a @WebACL@ .
wauAction :: Lens' WebACLUpdate ChangeAction
wauAction = lens _wauAction (\ s a -> s{_wauAction = a});

-- | The @ActivatedRule@ object in an 'UpdateWebACL' request specifies a @Rule@ that you want to insert or delete, the priority of the @Rule@ in the @WebACL@ , and the action that you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@ , @BLOCK@ , or @COUNT@ ).
wauActivatedRule :: Lens' WebACLUpdate ActivatedRule
wauActivatedRule = lens _wauActivatedRule (\ s a -> s{_wauActivatedRule = a});

instance Hashable WebACLUpdate

instance NFData WebACLUpdate

instance ToJSON WebACLUpdate where
        toJSON WebACLUpdate'{..}
          = object
              (catMaybes
                 [Just ("Action" .= _wauAction),
                  Just ("ActivatedRule" .= _wauActivatedRule)])

-- | A complex type that contains @XssMatchTuple@ objects, which specify the parts of web requests that you want AWS WAF to inspect for cross-site scripting attacks and, if you want AWS WAF to inspect a header, the name of the header. If a @XssMatchSet@ contains more than one @XssMatchTuple@ object, a request needs to include cross-site scripting attacks in only one of the specified parts of the request to be considered a match.
--
--
--
-- /See:/ 'xssMatchSet' smart constructor.
data XSSMatchSet = XSSMatchSet'
    { _xmsName           :: !(Maybe Text)
    , _xmsXSSMatchSetId  :: !Text
    , _xmsXSSMatchTuples :: ![XSSMatchTuple]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'XSSMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'xmsName' - The name, if any, of the @XssMatchSet@ .
--
-- * 'xmsXSSMatchSetId' - A unique identifier for an @XssMatchSet@ . You use @XssMatchSetId@ to get information about an @XssMatchSet@ (see 'GetXssMatchSet' ), update an @XssMatchSet@ (see 'UpdateXssMatchSet' ), insert an @XssMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @XssMatchSet@ from AWS WAF (see 'DeleteXssMatchSet' ). @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
--
-- * 'xmsXSSMatchTuples' - Specifies the parts of web requests that you want to inspect for cross-site scripting attacks.
xssMatchSet
    :: Text -- ^ 'xmsXSSMatchSetId'
    -> XSSMatchSet
xssMatchSet pXSSMatchSetId_ =
    XSSMatchSet'
    { _xmsName = Nothing
    , _xmsXSSMatchSetId = pXSSMatchSetId_
    , _xmsXSSMatchTuples = mempty
    }

-- | The name, if any, of the @XssMatchSet@ .
xmsName :: Lens' XSSMatchSet (Maybe Text)
xmsName = lens _xmsName (\ s a -> s{_xmsName = a});

-- | A unique identifier for an @XssMatchSet@ . You use @XssMatchSetId@ to get information about an @XssMatchSet@ (see 'GetXssMatchSet' ), update an @XssMatchSet@ (see 'UpdateXssMatchSet' ), insert an @XssMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @XssMatchSet@ from AWS WAF (see 'DeleteXssMatchSet' ). @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
xmsXSSMatchSetId :: Lens' XSSMatchSet Text
xmsXSSMatchSetId = lens _xmsXSSMatchSetId (\ s a -> s{_xmsXSSMatchSetId = a});

-- | Specifies the parts of web requests that you want to inspect for cross-site scripting attacks.
xmsXSSMatchTuples :: Lens' XSSMatchSet [XSSMatchTuple]
xmsXSSMatchTuples = lens _xmsXSSMatchTuples (\ s a -> s{_xmsXSSMatchTuples = a}) . _Coerce;

instance FromJSON XSSMatchSet where
        parseJSON
          = withObject "XSSMatchSet"
              (\ x ->
                 XSSMatchSet' <$>
                   (x .:? "Name") <*> (x .: "XssMatchSetId") <*>
                     (x .:? "XssMatchTuples" .!= mempty))

instance Hashable XSSMatchSet

instance NFData XSSMatchSet

-- | The @Id@ and @Name@ of an @XssMatchSet@ .
--
--
--
-- /See:/ 'xssMatchSetSummary' smart constructor.
data XSSMatchSetSummary = XSSMatchSetSummary'
    { _xmssXSSMatchSetId :: !Text
    , _xmssName          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'XSSMatchSetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'xmssXSSMatchSetId' - A unique identifier for an @XssMatchSet@ . You use @XssMatchSetId@ to get information about a @XssMatchSet@ (see 'GetXssMatchSet' ), update an @XssMatchSet@ (see 'UpdateXssMatchSet' ), insert an @XssMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @XssMatchSet@ from AWS WAF (see 'DeleteXssMatchSet' ). @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
--
-- * 'xmssName' - The name of the @XssMatchSet@ , if any, specified by @Id@ .
xssMatchSetSummary
    :: Text -- ^ 'xmssXSSMatchSetId'
    -> Text -- ^ 'xmssName'
    -> XSSMatchSetSummary
xssMatchSetSummary pXSSMatchSetId_ pName_ =
    XSSMatchSetSummary'
    { _xmssXSSMatchSetId = pXSSMatchSetId_
    , _xmssName = pName_
    }

-- | A unique identifier for an @XssMatchSet@ . You use @XssMatchSetId@ to get information about a @XssMatchSet@ (see 'GetXssMatchSet' ), update an @XssMatchSet@ (see 'UpdateXssMatchSet' ), insert an @XssMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @XssMatchSet@ from AWS WAF (see 'DeleteXssMatchSet' ). @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
xmssXSSMatchSetId :: Lens' XSSMatchSetSummary Text
xmssXSSMatchSetId = lens _xmssXSSMatchSetId (\ s a -> s{_xmssXSSMatchSetId = a});

-- | The name of the @XssMatchSet@ , if any, specified by @Id@ .
xmssName :: Lens' XSSMatchSetSummary Text
xmssName = lens _xmssName (\ s a -> s{_xmssName = a});

instance FromJSON XSSMatchSetSummary where
        parseJSON
          = withObject "XSSMatchSetSummary"
              (\ x ->
                 XSSMatchSetSummary' <$>
                   (x .: "XssMatchSetId") <*> (x .: "Name"))

instance Hashable XSSMatchSetSummary

instance NFData XSSMatchSetSummary

-- | Specifies the part of a web request that you want to inspect for cross-site scripting attacks and indicates whether you want to add the specification to an 'XssMatchSet' or delete it from an @XssMatchSet@ .
--
--
--
-- /See:/ 'xssMatchSetUpdate' smart constructor.
data XSSMatchSetUpdate = XSSMatchSetUpdate'
    { _xmsuAction        :: !ChangeAction
    , _xmsuXSSMatchTuple :: !XSSMatchTuple
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'XSSMatchSetUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'xmsuAction' - Specify @INSERT@ to add a 'XssMatchSetUpdate' to an 'XssMatchSet' . Use @DELETE@ to remove a @XssMatchSetUpdate@ from an @XssMatchSet@ .
--
-- * 'xmsuXSSMatchTuple' - Specifies the part of a web request that you want AWS WAF to inspect for cross-site scripting attacks and, if you want AWS WAF to inspect a header, the name of the header.
xssMatchSetUpdate
    :: ChangeAction -- ^ 'xmsuAction'
    -> XSSMatchTuple -- ^ 'xmsuXSSMatchTuple'
    -> XSSMatchSetUpdate
xssMatchSetUpdate pAction_ pXSSMatchTuple_ =
    XSSMatchSetUpdate'
    { _xmsuAction = pAction_
    , _xmsuXSSMatchTuple = pXSSMatchTuple_
    }

-- | Specify @INSERT@ to add a 'XssMatchSetUpdate' to an 'XssMatchSet' . Use @DELETE@ to remove a @XssMatchSetUpdate@ from an @XssMatchSet@ .
xmsuAction :: Lens' XSSMatchSetUpdate ChangeAction
xmsuAction = lens _xmsuAction (\ s a -> s{_xmsuAction = a});

-- | Specifies the part of a web request that you want AWS WAF to inspect for cross-site scripting attacks and, if you want AWS WAF to inspect a header, the name of the header.
xmsuXSSMatchTuple :: Lens' XSSMatchSetUpdate XSSMatchTuple
xmsuXSSMatchTuple = lens _xmsuXSSMatchTuple (\ s a -> s{_xmsuXSSMatchTuple = a});

instance Hashable XSSMatchSetUpdate

instance NFData XSSMatchSetUpdate

instance ToJSON XSSMatchSetUpdate where
        toJSON XSSMatchSetUpdate'{..}
          = object
              (catMaybes
                 [Just ("Action" .= _xmsuAction),
                  Just ("XssMatchTuple" .= _xmsuXSSMatchTuple)])

-- | Specifies the part of a web request that you want AWS WAF to inspect for cross-site scripting attacks and, if you want AWS WAF to inspect a header, the name of the header.
--
--
--
-- /See:/ 'xssMatchTuple' smart constructor.
data XSSMatchTuple = XSSMatchTuple'
    { _xmtFieldToMatch       :: !FieldToMatch
    , _xmtTextTransformation :: !TextTransformation
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'XSSMatchTuple' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'xmtFieldToMatch' - Specifies where in a web request to look for cross-site scripting attacks.
--
-- * 'xmtTextTransformation' - Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @FieldToMatch@ before inspecting a request for a match. __CMD_LINE__  When you're concerned that attackers are injecting an operating system commandline command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:     * Delete the following characters: \ " ' ^     * Delete spaces before the following characters: / (     * Replace the following characters with a space: , ;     * Replace multiple spaces with one space     * Convert uppercase letters (A-Z) to lowercase (a-z) __COMPRESS_WHITE_SPACE__  Use this option to replace the following characters with a space character (decimal 32):     * \f, formfeed, decimal 12     * \t, tab, decimal 9     * \n, newline, decimal 10     * \r, carriage return, decimal 13     * \v, vertical tab, decimal 11     * non-breaking space, decimal 160 @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space. __HTML_ENTITY_DECODE__  Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:     * Replaces @(ampersand)quot;@ with @"@      * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160     * Replaces @(ampersand)lt;@ with a "less than" symbol     * Replaces @(ampersand)gt;@ with @>@      * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters __LOWERCASE__  Use this option to convert uppercase letters (A-Z) to lowercase (a-z). __URL_DECODE__  Use this option to decode a URL-encoded value. __NONE__  Specify @NONE@ if you don't want to perform any text transformations.
xssMatchTuple
    :: FieldToMatch -- ^ 'xmtFieldToMatch'
    -> TextTransformation -- ^ 'xmtTextTransformation'
    -> XSSMatchTuple
xssMatchTuple pFieldToMatch_ pTextTransformation_ =
    XSSMatchTuple'
    { _xmtFieldToMatch = pFieldToMatch_
    , _xmtTextTransformation = pTextTransformation_
    }

-- | Specifies where in a web request to look for cross-site scripting attacks.
xmtFieldToMatch :: Lens' XSSMatchTuple FieldToMatch
xmtFieldToMatch = lens _xmtFieldToMatch (\ s a -> s{_xmtFieldToMatch = a});

-- | Text transformations eliminate some of the unusual formatting that attackers use in web requests in an effort to bypass AWS WAF. If you specify a transformation, AWS WAF performs the transformation on @FieldToMatch@ before inspecting a request for a match. __CMD_LINE__  When you're concerned that attackers are injecting an operating system commandline command and using unusual formatting to disguise some or all of the command, use this option to perform the following transformations:     * Delete the following characters: \ " ' ^     * Delete spaces before the following characters: / (     * Replace the following characters with a space: , ;     * Replace multiple spaces with one space     * Convert uppercase letters (A-Z) to lowercase (a-z) __COMPRESS_WHITE_SPACE__  Use this option to replace the following characters with a space character (decimal 32):     * \f, formfeed, decimal 12     * \t, tab, decimal 9     * \n, newline, decimal 10     * \r, carriage return, decimal 13     * \v, vertical tab, decimal 11     * non-breaking space, decimal 160 @COMPRESS_WHITE_SPACE@ also replaces multiple spaces with one space. __HTML_ENTITY_DECODE__  Use this option to replace HTML-encoded characters with unencoded characters. @HTML_ENTITY_DECODE@ performs the following operations:     * Replaces @(ampersand)quot;@ with @"@      * Replaces @(ampersand)nbsp;@ with a non-breaking space, decimal 160     * Replaces @(ampersand)lt;@ with a "less than" symbol     * Replaces @(ampersand)gt;@ with @>@      * Replaces characters that are represented in hexadecimal format, @(ampersand)#xhhhh;@ , with the corresponding characters     * Replaces characters that are represented in decimal format, @(ampersand)#nnnn;@ , with the corresponding characters __LOWERCASE__  Use this option to convert uppercase letters (A-Z) to lowercase (a-z). __URL_DECODE__  Use this option to decode a URL-encoded value. __NONE__  Specify @NONE@ if you don't want to perform any text transformations.
xmtTextTransformation :: Lens' XSSMatchTuple TextTransformation
xmtTextTransformation = lens _xmtTextTransformation (\ s a -> s{_xmtTextTransformation = a});

instance FromJSON XSSMatchTuple where
        parseJSON
          = withObject "XSSMatchTuple"
              (\ x ->
                 XSSMatchTuple' <$>
                   (x .: "FieldToMatch") <*>
                     (x .: "TextTransformation"))

instance Hashable XSSMatchTuple

instance NFData XSSMatchTuple

instance ToJSON XSSMatchTuple where
        toJSON XSSMatchTuple'{..}
          = object
              (catMaybes
                 [Just ("FieldToMatch" .= _xmtFieldToMatch),
                  Just
                    ("TextTransformation" .= _xmtTextTransformation)])

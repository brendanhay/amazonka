{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.UpdateWebACL
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'ActivatedRule' objects in a @WebACL@ . Each @Rule@ identifies web requests that you want to allow, block, or count. When you update a @WebACL@ , you specify the following values:
--
--
--     * A default action for the @WebACL@ , either @ALLOW@ or @BLOCK@ . AWS WAF performs the default action if a request doesn't match the criteria in any of the @Rules@ in a @WebACL@ .
--
--     * The @Rules@ that you want to add and/or delete. If you want to replace one @Rule@ with another, you delete the existing @Rule@ and add the new one.
--
--     * For each @Rule@ , whether you want AWS WAF to allow requests, block requests, or count requests that match the conditions in the @Rule@ .
--
--     * The order in which you want AWS WAF to evaluate the @Rules@ in a @WebACL@ . If you add more than one @Rule@ to a @WebACL@ , AWS WAF evaluates each request against the @Rules@ in order based on the value of @Priority@ . (The @Rule@ that has the lowest value for @Priority@ is evaluated first.) When a web request matches all of the predicates (such as @ByteMatchSets@ and @IPSets@ ) in a @Rule@ , AWS WAF immediately takes the corresponding action, allow or block, and doesn't evaluate the request against the remaining @Rules@ in the @WebACL@ , if any.
--
--
--
-- To create and configure a @WebACL@ , perform the following steps:
--
--     * Create and update the predicates that you want to include in @Rules@ . For more information, see 'CreateByteMatchSet' , 'UpdateByteMatchSet' , 'CreateIPSet' , 'UpdateIPSet' , 'CreateSqlInjectionMatchSet' , and 'UpdateSqlInjectionMatchSet' .
--
--     * Create and update the @Rules@ that you want to include in the @WebACL@ . For more information, see 'CreateRule' and 'UpdateRule' .
--
--     * Create a @WebACL@ . See 'CreateWebACL' .
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateWebACL' request.
--
--     * Submit an @UpdateWebACL@ request to specify the @Rules@ that you want to include in the @WebACL@ , to specify the default action, and to associate the @WebACL@ with a CloudFront distribution.
--
--
--
-- Be aware that if you try to add a RATE_BASED rule to a web ACL without setting the rule type when first creating the rule, the 'UpdateWebACL' request will fail because the request tries to add a REGULAR rule (the default rule type) with the specified ID, which does not exist.
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
--
module Network.AWS.WAFRegional.UpdateWebACL
    (
    -- * Creating a Request
      updateWebACL
    , UpdateWebACL
    -- * Request Lenses
    , uwaUpdates
    , uwaDefaultAction
    , uwaWebACLId
    , uwaChangeToken

    -- * Destructuring the Response
    , updateWebACLResponse
    , UpdateWebACLResponse
    -- * Response Lenses
    , uwarsChangeToken
    , uwarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'updateWebACL' smart constructor.
data UpdateWebACL = UpdateWebACL'
  { _uwaUpdates       :: !(Maybe [WebACLUpdate])
  , _uwaDefaultAction :: !(Maybe WafAction)
  , _uwaWebACLId      :: !Text
  , _uwaChangeToken   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateWebACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uwaUpdates' - An array of updates to make to the 'WebACL' . An array of @WebACLUpdate@ objects that you want to insert into or delete from a 'WebACL' . For more information, see the applicable data types:     * 'WebACLUpdate' : Contains @Action@ and @ActivatedRule@      * 'ActivatedRule' : Contains @Action@ , @OverrideAction@ , @Priority@ , @RuleId@ , and @Type@ . @ActivatedRule|OverrideAction@ applies only when updating or adding a @RuleGroup@ to a @WebACL@ . In this case you do not use @ActivatedRule|Action@ . For all other update requests, @ActivatedRule|Action@ is used instead of @ActivatedRule|OverrideAction@ .      * 'WafAction' : Contains @Type@
--
-- * 'uwaDefaultAction' - A default action for the web ACL, either ALLOW or BLOCK. AWS WAF performs the default action if a request doesn't match the criteria in any of the rules in a web ACL.
--
-- * 'uwaWebACLId' - The @WebACLId@ of the 'WebACL' that you want to update. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
--
-- * 'uwaChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
updateWebACL
    :: Text -- ^ 'uwaWebACLId'
    -> Text -- ^ 'uwaChangeToken'
    -> UpdateWebACL
updateWebACL pWebACLId_ pChangeToken_ =
  UpdateWebACL'
    { _uwaUpdates = Nothing
    , _uwaDefaultAction = Nothing
    , _uwaWebACLId = pWebACLId_
    , _uwaChangeToken = pChangeToken_
    }


-- | An array of updates to make to the 'WebACL' . An array of @WebACLUpdate@ objects that you want to insert into or delete from a 'WebACL' . For more information, see the applicable data types:     * 'WebACLUpdate' : Contains @Action@ and @ActivatedRule@      * 'ActivatedRule' : Contains @Action@ , @OverrideAction@ , @Priority@ , @RuleId@ , and @Type@ . @ActivatedRule|OverrideAction@ applies only when updating or adding a @RuleGroup@ to a @WebACL@ . In this case you do not use @ActivatedRule|Action@ . For all other update requests, @ActivatedRule|Action@ is used instead of @ActivatedRule|OverrideAction@ .      * 'WafAction' : Contains @Type@
uwaUpdates :: Lens' UpdateWebACL [WebACLUpdate]
uwaUpdates = lens _uwaUpdates (\ s a -> s{_uwaUpdates = a}) . _Default . _Coerce

-- | A default action for the web ACL, either ALLOW or BLOCK. AWS WAF performs the default action if a request doesn't match the criteria in any of the rules in a web ACL.
uwaDefaultAction :: Lens' UpdateWebACL (Maybe WafAction)
uwaDefaultAction = lens _uwaDefaultAction (\ s a -> s{_uwaDefaultAction = a})

-- | The @WebACLId@ of the 'WebACL' that you want to update. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
uwaWebACLId :: Lens' UpdateWebACL Text
uwaWebACLId = lens _uwaWebACLId (\ s a -> s{_uwaWebACLId = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
uwaChangeToken :: Lens' UpdateWebACL Text
uwaChangeToken = lens _uwaChangeToken (\ s a -> s{_uwaChangeToken = a})

instance AWSRequest UpdateWebACL where
        type Rs UpdateWebACL = UpdateWebACLResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 UpdateWebACLResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable UpdateWebACL where

instance NFData UpdateWebACL where

instance ToHeaders UpdateWebACL where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.UpdateWebACL" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateWebACL where
        toJSON UpdateWebACL'{..}
          = object
              (catMaybes
                 [("Updates" .=) <$> _uwaUpdates,
                  ("DefaultAction" .=) <$> _uwaDefaultAction,
                  Just ("WebACLId" .= _uwaWebACLId),
                  Just ("ChangeToken" .= _uwaChangeToken)])

instance ToPath UpdateWebACL where
        toPath = const "/"

instance ToQuery UpdateWebACL where
        toQuery = const mempty

-- | /See:/ 'updateWebACLResponse' smart constructor.
data UpdateWebACLResponse = UpdateWebACLResponse'
  { _uwarsChangeToken    :: !(Maybe Text)
  , _uwarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateWebACLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uwarsChangeToken' - The @ChangeToken@ that you used to submit the @UpdateWebACL@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'uwarsResponseStatus' - -- | The response status code.
updateWebACLResponse
    :: Int -- ^ 'uwarsResponseStatus'
    -> UpdateWebACLResponse
updateWebACLResponse pResponseStatus_ =
  UpdateWebACLResponse'
    {_uwarsChangeToken = Nothing, _uwarsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @UpdateWebACL@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
uwarsChangeToken :: Lens' UpdateWebACLResponse (Maybe Text)
uwarsChangeToken = lens _uwarsChangeToken (\ s a -> s{_uwarsChangeToken = a})

-- | -- | The response status code.
uwarsResponseStatus :: Lens' UpdateWebACLResponse Int
uwarsResponseStatus = lens _uwarsResponseStatus (\ s a -> s{_uwarsResponseStatus = a})

instance NFData UpdateWebACLResponse where

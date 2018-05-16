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
-- Module      : Network.AWS.WAF.CreateWebACL
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @WebACL@ , which contains the @Rules@ that identify the CloudFront web requests that you want to allow, block, or count. AWS WAF evaluates @Rules@ in order based on the value of @Priority@ for each @Rule@ .
--
--
-- You also specify a default action, either @ALLOW@ or @BLOCK@ . If a web request doesn't match any of the @Rules@ in a @WebACL@ , AWS WAF responds to the request with the default action.
--
-- To create and configure a @WebACL@ , perform the following steps:
--
--     * Create and update the @ByteMatchSet@ objects and other predicates that you want to include in @Rules@ . For more information, see 'CreateByteMatchSet' , 'UpdateByteMatchSet' , 'CreateIPSet' , 'UpdateIPSet' , 'CreateSqlInjectionMatchSet' , and 'UpdateSqlInjectionMatchSet' .
--
--     * Create and update the @Rules@ that you want to include in the @WebACL@ . For more information, see 'CreateRule' and 'UpdateRule' .
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateWebACL@ request.
--
--     * Submit a @CreateWebACL@ request.
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateWebACL' request.
--
--     * Submit an 'UpdateWebACL' request to specify the @Rules@ that you want to include in the @WebACL@ , to specify the default action, and to associate the @WebACL@ with a CloudFront distribution.
--
--
--
-- For more information about how to use the AWS WAF API, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
--
module Network.AWS.WAF.CreateWebACL
    (
    -- * Creating a Request
      createWebACL
    , CreateWebACL
    -- * Request Lenses
    , cwaName
    , cwaMetricName
    , cwaDefaultAction
    , cwaChangeToken

    -- * Destructuring the Response
    , createWebACLResponse
    , CreateWebACLResponse
    -- * Response Lenses
    , cwarsWebACL
    , cwarsChangeToken
    , cwarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'createWebACL' smart constructor.
data CreateWebACL = CreateWebACL'
  { _cwaName          :: !Text
  , _cwaMetricName    :: !Text
  , _cwaDefaultAction :: !WafAction
  , _cwaChangeToken   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateWebACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwaName' - A friendly name or description of the 'WebACL' . You can't change @Name@ after you create the @WebACL@ .
--
-- * 'cwaMetricName' - A friendly name or description for the metrics for this @WebACL@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9); the name can't contain whitespace. You can't change @MetricName@ after you create the @WebACL@ .
--
-- * 'cwaDefaultAction' - The action that you want AWS WAF to take when a request doesn't match the criteria specified in any of the @Rule@ objects that are associated with the @WebACL@ .
--
-- * 'cwaChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
createWebACL
    :: Text -- ^ 'cwaName'
    -> Text -- ^ 'cwaMetricName'
    -> WafAction -- ^ 'cwaDefaultAction'
    -> Text -- ^ 'cwaChangeToken'
    -> CreateWebACL
createWebACL pName_ pMetricName_ pDefaultAction_ pChangeToken_ =
  CreateWebACL'
    { _cwaName = pName_
    , _cwaMetricName = pMetricName_
    , _cwaDefaultAction = pDefaultAction_
    , _cwaChangeToken = pChangeToken_
    }


-- | A friendly name or description of the 'WebACL' . You can't change @Name@ after you create the @WebACL@ .
cwaName :: Lens' CreateWebACL Text
cwaName = lens _cwaName (\ s a -> s{_cwaName = a})

-- | A friendly name or description for the metrics for this @WebACL@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9); the name can't contain whitespace. You can't change @MetricName@ after you create the @WebACL@ .
cwaMetricName :: Lens' CreateWebACL Text
cwaMetricName = lens _cwaMetricName (\ s a -> s{_cwaMetricName = a})

-- | The action that you want AWS WAF to take when a request doesn't match the criteria specified in any of the @Rule@ objects that are associated with the @WebACL@ .
cwaDefaultAction :: Lens' CreateWebACL WafAction
cwaDefaultAction = lens _cwaDefaultAction (\ s a -> s{_cwaDefaultAction = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
cwaChangeToken :: Lens' CreateWebACL Text
cwaChangeToken = lens _cwaChangeToken (\ s a -> s{_cwaChangeToken = a})

instance AWSRequest CreateWebACL where
        type Rs CreateWebACL = CreateWebACLResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 CreateWebACLResponse' <$>
                   (x .?> "WebACL") <*> (x .?> "ChangeToken") <*>
                     (pure (fromEnum s)))

instance Hashable CreateWebACL where

instance NFData CreateWebACL where

instance ToHeaders CreateWebACL where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.CreateWebACL" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateWebACL where
        toJSON CreateWebACL'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _cwaName),
                  Just ("MetricName" .= _cwaMetricName),
                  Just ("DefaultAction" .= _cwaDefaultAction),
                  Just ("ChangeToken" .= _cwaChangeToken)])

instance ToPath CreateWebACL where
        toPath = const "/"

instance ToQuery CreateWebACL where
        toQuery = const mempty

-- | /See:/ 'createWebACLResponse' smart constructor.
data CreateWebACLResponse = CreateWebACLResponse'
  { _cwarsWebACL         :: !(Maybe WebACL)
  , _cwarsChangeToken    :: !(Maybe Text)
  , _cwarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateWebACLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwarsWebACL' - The 'WebACL' returned in the @CreateWebACL@ response.
--
-- * 'cwarsChangeToken' - The @ChangeToken@ that you used to submit the @CreateWebACL@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'cwarsResponseStatus' - -- | The response status code.
createWebACLResponse
    :: Int -- ^ 'cwarsResponseStatus'
    -> CreateWebACLResponse
createWebACLResponse pResponseStatus_ =
  CreateWebACLResponse'
    { _cwarsWebACL = Nothing
    , _cwarsChangeToken = Nothing
    , _cwarsResponseStatus = pResponseStatus_
    }


-- | The 'WebACL' returned in the @CreateWebACL@ response.
cwarsWebACL :: Lens' CreateWebACLResponse (Maybe WebACL)
cwarsWebACL = lens _cwarsWebACL (\ s a -> s{_cwarsWebACL = a})

-- | The @ChangeToken@ that you used to submit the @CreateWebACL@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
cwarsChangeToken :: Lens' CreateWebACLResponse (Maybe Text)
cwarsChangeToken = lens _cwarsChangeToken (\ s a -> s{_cwarsChangeToken = a})

-- | -- | The response status code.
cwarsResponseStatus :: Lens' CreateWebACLResponse Int
cwarsResponseStatus = lens _cwarsResponseStatus (\ s a -> s{_cwarsResponseStatus = a})

instance NFData CreateWebACLResponse where

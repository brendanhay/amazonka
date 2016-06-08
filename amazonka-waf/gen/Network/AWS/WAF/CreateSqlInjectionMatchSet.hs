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
-- Module      : Network.AWS.WAF.CreateSqlInjectionMatchSet
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a < SqlInjectionMatchSet>, which you use to allow, block, or count requests that contain snippets of SQL code in a specified part of web requests. AWS WAF searches for character sequences that are likely to be malicious strings.
--
-- To create and configure a 'SqlInjectionMatchSet', perform the following steps:
--
-- 1.  Use < GetChangeToken> to get the change token that you provide in the 'ChangeToken' parameter of a 'CreateSqlInjectionMatchSet' request.
-- 2.  Submit a 'CreateSqlInjectionMatchSet' request.
-- 3.  Use 'GetChangeToken' to get the change token that you provide in the 'ChangeToken' parameter of an < UpdateSqlInjectionMatchSet> request.
-- 4.  Submit an < UpdateSqlInjectionMatchSet> request to specify the parts of web requests in which you want to allow, block, or count malicious SQL code.
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Network.AWS.WAF.CreateSqlInjectionMatchSet
    (
    -- * Creating a Request
      createSqlInjectionMatchSet
    , CreateSqlInjectionMatchSet
    -- * Request Lenses
    , csimsName
    , csimsChangeToken

    -- * Destructuring the Response
    , createSqlInjectionMatchSetResponse
    , CreateSqlInjectionMatchSetResponse
    -- * Response Lenses
    , csimsrsSqlInjectionMatchSet
    , csimsrsChangeToken
    , csimsrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WAF.Types
import           Network.AWS.WAF.Types.Product

-- | A request to create a < SqlInjectionMatchSet>.
--
-- /See:/ 'createSqlInjectionMatchSet' smart constructor.
data CreateSqlInjectionMatchSet = CreateSqlInjectionMatchSet'
    { _csimsName        :: !Text
    , _csimsChangeToken :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateSqlInjectionMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csimsName'
--
-- * 'csimsChangeToken'
createSqlInjectionMatchSet
    :: Text -- ^ 'csimsName'
    -> Text -- ^ 'csimsChangeToken'
    -> CreateSqlInjectionMatchSet
createSqlInjectionMatchSet pName_ pChangeToken_ =
    CreateSqlInjectionMatchSet'
    { _csimsName = pName_
    , _csimsChangeToken = pChangeToken_
    }

-- | A friendly name or description for the < SqlInjectionMatchSet> that you\'re creating. You can\'t change 'Name' after you create the 'SqlInjectionMatchSet'.
csimsName :: Lens' CreateSqlInjectionMatchSet Text
csimsName = lens _csimsName (\ s a -> s{_csimsName = a});

-- | The value returned by the most recent call to < GetChangeToken>.
csimsChangeToken :: Lens' CreateSqlInjectionMatchSet Text
csimsChangeToken = lens _csimsChangeToken (\ s a -> s{_csimsChangeToken = a});

instance AWSRequest CreateSqlInjectionMatchSet where
        type Rs CreateSqlInjectionMatchSet =
             CreateSqlInjectionMatchSetResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 CreateSqlInjectionMatchSetResponse' <$>
                   (x .?> "SqlInjectionMatchSet") <*>
                     (x .?> "ChangeToken")
                     <*> (pure (fromEnum s)))

instance Hashable CreateSqlInjectionMatchSet

instance NFData CreateSqlInjectionMatchSet

instance ToHeaders CreateSqlInjectionMatchSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.CreateSqlInjectionMatchSet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateSqlInjectionMatchSet where
        toJSON CreateSqlInjectionMatchSet'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _csimsName),
                  Just ("ChangeToken" .= _csimsChangeToken)])

instance ToPath CreateSqlInjectionMatchSet where
        toPath = const "/"

instance ToQuery CreateSqlInjectionMatchSet where
        toQuery = const mempty

-- | The response to a 'CreateSqlInjectionMatchSet' request.
--
-- /See:/ 'createSqlInjectionMatchSetResponse' smart constructor.
data CreateSqlInjectionMatchSetResponse = CreateSqlInjectionMatchSetResponse'
    { _csimsrsSqlInjectionMatchSet :: !(Maybe SqlInjectionMatchSet)
    , _csimsrsChangeToken          :: !(Maybe Text)
    , _csimsrsResponseStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateSqlInjectionMatchSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csimsrsSqlInjectionMatchSet'
--
-- * 'csimsrsChangeToken'
--
-- * 'csimsrsResponseStatus'
createSqlInjectionMatchSetResponse
    :: Int -- ^ 'csimsrsResponseStatus'
    -> CreateSqlInjectionMatchSetResponse
createSqlInjectionMatchSetResponse pResponseStatus_ =
    CreateSqlInjectionMatchSetResponse'
    { _csimsrsSqlInjectionMatchSet = Nothing
    , _csimsrsChangeToken = Nothing
    , _csimsrsResponseStatus = pResponseStatus_
    }

-- | A < SqlInjectionMatchSet>.
csimsrsSqlInjectionMatchSet :: Lens' CreateSqlInjectionMatchSetResponse (Maybe SqlInjectionMatchSet)
csimsrsSqlInjectionMatchSet = lens _csimsrsSqlInjectionMatchSet (\ s a -> s{_csimsrsSqlInjectionMatchSet = a});

-- | The 'ChangeToken' that you used to submit the 'CreateSqlInjectionMatchSet' request. You can also use this value to query the status of the request. For more information, see < GetChangeTokenStatus>.
csimsrsChangeToken :: Lens' CreateSqlInjectionMatchSetResponse (Maybe Text)
csimsrsChangeToken = lens _csimsrsChangeToken (\ s a -> s{_csimsrsChangeToken = a});

-- | The response status code.
csimsrsResponseStatus :: Lens' CreateSqlInjectionMatchSetResponse Int
csimsrsResponseStatus = lens _csimsrsResponseStatus (\ s a -> s{_csimsrsResponseStatus = a});

instance NFData CreateSqlInjectionMatchSetResponse

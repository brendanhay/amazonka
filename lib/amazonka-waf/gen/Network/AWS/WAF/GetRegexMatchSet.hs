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
-- Module      : Network.AWS.WAF.GetRegexMatchSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'RegexMatchSet' specified by @RegexMatchSetId@ .
--
--
module Network.AWS.WAF.GetRegexMatchSet
    (
    -- * Creating a Request
      getRegexMatchSet
    , GetRegexMatchSet
    -- * Request Lenses
    , grmsRegexMatchSetId

    -- * Destructuring the Response
    , getRegexMatchSetResponse
    , GetRegexMatchSetResponse
    -- * Response Lenses
    , grmsrsRegexMatchSet
    , grmsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'getRegexMatchSet' smart constructor.
newtype GetRegexMatchSet = GetRegexMatchSet'
  { _grmsRegexMatchSetId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRegexMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grmsRegexMatchSetId' - The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to get. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
getRegexMatchSet
    :: Text -- ^ 'grmsRegexMatchSetId'
    -> GetRegexMatchSet
getRegexMatchSet pRegexMatchSetId_ =
  GetRegexMatchSet' {_grmsRegexMatchSetId = pRegexMatchSetId_}


-- | The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to get. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
grmsRegexMatchSetId :: Lens' GetRegexMatchSet Text
grmsRegexMatchSetId = lens _grmsRegexMatchSetId (\ s a -> s{_grmsRegexMatchSetId = a})

instance AWSRequest GetRegexMatchSet where
        type Rs GetRegexMatchSet = GetRegexMatchSetResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 GetRegexMatchSetResponse' <$>
                   (x .?> "RegexMatchSet") <*> (pure (fromEnum s)))

instance Hashable GetRegexMatchSet where

instance NFData GetRegexMatchSet where

instance ToHeaders GetRegexMatchSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.GetRegexMatchSet" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRegexMatchSet where
        toJSON GetRegexMatchSet'{..}
          = object
              (catMaybes
                 [Just ("RegexMatchSetId" .= _grmsRegexMatchSetId)])

instance ToPath GetRegexMatchSet where
        toPath = const "/"

instance ToQuery GetRegexMatchSet where
        toQuery = const mempty

-- | /See:/ 'getRegexMatchSetResponse' smart constructor.
data GetRegexMatchSetResponse = GetRegexMatchSetResponse'
  { _grmsrsRegexMatchSet  :: !(Maybe RegexMatchSet)
  , _grmsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRegexMatchSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grmsrsRegexMatchSet' - Information about the 'RegexMatchSet' that you specified in the @GetRegexMatchSet@ request. For more information, see 'RegexMatchTuple' .
--
-- * 'grmsrsResponseStatus' - -- | The response status code.
getRegexMatchSetResponse
    :: Int -- ^ 'grmsrsResponseStatus'
    -> GetRegexMatchSetResponse
getRegexMatchSetResponse pResponseStatus_ =
  GetRegexMatchSetResponse'
    {_grmsrsRegexMatchSet = Nothing, _grmsrsResponseStatus = pResponseStatus_}


-- | Information about the 'RegexMatchSet' that you specified in the @GetRegexMatchSet@ request. For more information, see 'RegexMatchTuple' .
grmsrsRegexMatchSet :: Lens' GetRegexMatchSetResponse (Maybe RegexMatchSet)
grmsrsRegexMatchSet = lens _grmsrsRegexMatchSet (\ s a -> s{_grmsrsRegexMatchSet = a})

-- | -- | The response status code.
grmsrsResponseStatus :: Lens' GetRegexMatchSetResponse Int
grmsrsResponseStatus = lens _grmsrsResponseStatus (\ s a -> s{_grmsrsResponseStatus = a})

instance NFData GetRegexMatchSetResponse where

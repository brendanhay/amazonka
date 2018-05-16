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
-- Module      : Network.AWS.WAFRegional.GetByteMatchSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'ByteMatchSet' specified by @ByteMatchSetId@ .
--
--
module Network.AWS.WAFRegional.GetByteMatchSet
    (
    -- * Creating a Request
      getByteMatchSet
    , GetByteMatchSet
    -- * Request Lenses
    , gbmsByteMatchSetId

    -- * Destructuring the Response
    , getByteMatchSetResponse
    , GetByteMatchSetResponse
    -- * Response Lenses
    , gbmsrsByteMatchSet
    , gbmsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'getByteMatchSet' smart constructor.
newtype GetByteMatchSet = GetByteMatchSet'
  { _gbmsByteMatchSetId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetByteMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbmsByteMatchSetId' - The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to get. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
getByteMatchSet
    :: Text -- ^ 'gbmsByteMatchSetId'
    -> GetByteMatchSet
getByteMatchSet pByteMatchSetId_ =
  GetByteMatchSet' {_gbmsByteMatchSetId = pByteMatchSetId_}


-- | The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to get. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
gbmsByteMatchSetId :: Lens' GetByteMatchSet Text
gbmsByteMatchSetId = lens _gbmsByteMatchSetId (\ s a -> s{_gbmsByteMatchSetId = a})

instance AWSRequest GetByteMatchSet where
        type Rs GetByteMatchSet = GetByteMatchSetResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 GetByteMatchSetResponse' <$>
                   (x .?> "ByteMatchSet") <*> (pure (fromEnum s)))

instance Hashable GetByteMatchSet where

instance NFData GetByteMatchSet where

instance ToHeaders GetByteMatchSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.GetByteMatchSet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetByteMatchSet where
        toJSON GetByteMatchSet'{..}
          = object
              (catMaybes
                 [Just ("ByteMatchSetId" .= _gbmsByteMatchSetId)])

instance ToPath GetByteMatchSet where
        toPath = const "/"

instance ToQuery GetByteMatchSet where
        toQuery = const mempty

-- | /See:/ 'getByteMatchSetResponse' smart constructor.
data GetByteMatchSetResponse = GetByteMatchSetResponse'
  { _gbmsrsByteMatchSet   :: !(Maybe ByteMatchSet)
  , _gbmsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetByteMatchSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbmsrsByteMatchSet' - Information about the 'ByteMatchSet' that you specified in the @GetByteMatchSet@ request. For more information, see the following topics:     * 'ByteMatchSet' : Contains @ByteMatchSetId@ , @ByteMatchTuples@ , and @Name@      * @ByteMatchTuples@ : Contains an array of 'ByteMatchTuple' objects. Each @ByteMatchTuple@ object contains 'FieldToMatch' , @PositionalConstraint@ , @TargetString@ , and @TextTransformation@      * 'FieldToMatch' : Contains @Data@ and @Type@
--
-- * 'gbmsrsResponseStatus' - -- | The response status code.
getByteMatchSetResponse
    :: Int -- ^ 'gbmsrsResponseStatus'
    -> GetByteMatchSetResponse
getByteMatchSetResponse pResponseStatus_ =
  GetByteMatchSetResponse'
    {_gbmsrsByteMatchSet = Nothing, _gbmsrsResponseStatus = pResponseStatus_}


-- | Information about the 'ByteMatchSet' that you specified in the @GetByteMatchSet@ request. For more information, see the following topics:     * 'ByteMatchSet' : Contains @ByteMatchSetId@ , @ByteMatchTuples@ , and @Name@      * @ByteMatchTuples@ : Contains an array of 'ByteMatchTuple' objects. Each @ByteMatchTuple@ object contains 'FieldToMatch' , @PositionalConstraint@ , @TargetString@ , and @TextTransformation@      * 'FieldToMatch' : Contains @Data@ and @Type@
gbmsrsByteMatchSet :: Lens' GetByteMatchSetResponse (Maybe ByteMatchSet)
gbmsrsByteMatchSet = lens _gbmsrsByteMatchSet (\ s a -> s{_gbmsrsByteMatchSet = a})

-- | -- | The response status code.
gbmsrsResponseStatus :: Lens' GetByteMatchSetResponse Int
gbmsrsResponseStatus = lens _gbmsrsResponseStatus (\ s a -> s{_gbmsrsResponseStatus = a})

instance NFData GetByteMatchSetResponse where

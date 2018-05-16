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
-- Module      : Network.AWS.CloudTrail.ListPublicKeys
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all public keys whose private keys were used to sign the digest files within the specified time range. The public key is needed to validate digest files that were signed with its corresponding private key.
--
--
module Network.AWS.CloudTrail.ListPublicKeys
    (
    -- * Creating a Request
      listPublicKeys
    , ListPublicKeys
    -- * Request Lenses
    , lpkStartTime
    , lpkNextToken
    , lpkEndTime

    -- * Destructuring the Response
    , listPublicKeysResponse
    , ListPublicKeysResponse
    -- * Response Lenses
    , lpkrsPublicKeyList
    , lpkrsNextToken
    , lpkrsResponseStatus
    ) where

import Network.AWS.CloudTrail.Types
import Network.AWS.CloudTrail.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Requests the public keys for a specified time range.
--
--
--
-- /See:/ 'listPublicKeys' smart constructor.
data ListPublicKeys = ListPublicKeys'
  { _lpkStartTime :: !(Maybe POSIX)
  , _lpkNextToken :: !(Maybe Text)
  , _lpkEndTime   :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPublicKeys' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpkStartTime' - Optionally specifies, in UTC, the start of the time range to look up public keys for CloudTrail digest files. If not specified, the current time is used, and the current public key is returned.
--
-- * 'lpkNextToken' - Reserved for future use.
--
-- * 'lpkEndTime' - Optionally specifies, in UTC, the end of the time range to look up public keys for CloudTrail digest files. If not specified, the current time is used.
listPublicKeys
    :: ListPublicKeys
listPublicKeys =
  ListPublicKeys'
    {_lpkStartTime = Nothing, _lpkNextToken = Nothing, _lpkEndTime = Nothing}


-- | Optionally specifies, in UTC, the start of the time range to look up public keys for CloudTrail digest files. If not specified, the current time is used, and the current public key is returned.
lpkStartTime :: Lens' ListPublicKeys (Maybe UTCTime)
lpkStartTime = lens _lpkStartTime (\ s a -> s{_lpkStartTime = a}) . mapping _Time

-- | Reserved for future use.
lpkNextToken :: Lens' ListPublicKeys (Maybe Text)
lpkNextToken = lens _lpkNextToken (\ s a -> s{_lpkNextToken = a})

-- | Optionally specifies, in UTC, the end of the time range to look up public keys for CloudTrail digest files. If not specified, the current time is used.
lpkEndTime :: Lens' ListPublicKeys (Maybe UTCTime)
lpkEndTime = lens _lpkEndTime (\ s a -> s{_lpkEndTime = a}) . mapping _Time

instance AWSRequest ListPublicKeys where
        type Rs ListPublicKeys = ListPublicKeysResponse
        request = postJSON cloudTrail
        response
          = receiveJSON
              (\ s h x ->
                 ListPublicKeysResponse' <$>
                   (x .?> "PublicKeyList" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListPublicKeys where

instance NFData ListPublicKeys where

instance ToHeaders ListPublicKeys where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.ListPublicKeys"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListPublicKeys where
        toJSON ListPublicKeys'{..}
          = object
              (catMaybes
                 [("StartTime" .=) <$> _lpkStartTime,
                  ("NextToken" .=) <$> _lpkNextToken,
                  ("EndTime" .=) <$> _lpkEndTime])

instance ToPath ListPublicKeys where
        toPath = const "/"

instance ToQuery ListPublicKeys where
        toQuery = const mempty

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
--
--
-- /See:/ 'listPublicKeysResponse' smart constructor.
data ListPublicKeysResponse = ListPublicKeysResponse'
  { _lpkrsPublicKeyList  :: !(Maybe [PublicKey])
  , _lpkrsNextToken      :: !(Maybe Text)
  , _lpkrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPublicKeysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpkrsPublicKeyList' - Contains an array of PublicKey objects.
--
-- * 'lpkrsNextToken' - Reserved for future use.
--
-- * 'lpkrsResponseStatus' - -- | The response status code.
listPublicKeysResponse
    :: Int -- ^ 'lpkrsResponseStatus'
    -> ListPublicKeysResponse
listPublicKeysResponse pResponseStatus_ =
  ListPublicKeysResponse'
    { _lpkrsPublicKeyList = Nothing
    , _lpkrsNextToken = Nothing
    , _lpkrsResponseStatus = pResponseStatus_
    }


-- | Contains an array of PublicKey objects.
lpkrsPublicKeyList :: Lens' ListPublicKeysResponse [PublicKey]
lpkrsPublicKeyList = lens _lpkrsPublicKeyList (\ s a -> s{_lpkrsPublicKeyList = a}) . _Default . _Coerce

-- | Reserved for future use.
lpkrsNextToken :: Lens' ListPublicKeysResponse (Maybe Text)
lpkrsNextToken = lens _lpkrsNextToken (\ s a -> s{_lpkrsNextToken = a})

-- | -- | The response status code.
lpkrsResponseStatus :: Lens' ListPublicKeysResponse Int
lpkrsResponseStatus = lens _lpkrsResponseStatus (\ s a -> s{_lpkrsResponseStatus = a})

instance NFData ListPublicKeysResponse where

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
-- Module      : Network.AWS.QLDB.GetDigest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.QLDB.GetDigest
    (
    -- * Creating a Request
      getDigest
    , GetDigest
    -- * Request Lenses
    , gdName

    -- * Destructuring the Response
    , getDigestResponse
    , GetDigestResponse
    -- * Response Lenses
    , gdrsResponseStatus
    , gdrsDigest
    , gdrsDigestTipAddress
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.QLDB.Types
import Network.AWS.QLDB.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDigest' smart constructor.
newtype GetDigest = GetDigest'
  { _gdName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDigest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdName' - Undocumented member.
getDigest
    :: Text -- ^ 'gdName'
    -> GetDigest
getDigest pName_ = GetDigest' {_gdName = pName_}


-- | Undocumented member.
gdName :: Lens' GetDigest Text
gdName = lens _gdName (\ s a -> s{_gdName = a})

instance AWSRequest GetDigest where
        type Rs GetDigest = GetDigestResponse
        request = postJSON qldb
        response
          = receiveJSON
              (\ s h x ->
                 GetDigestResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "Digest") <*>
                     (x .:> "DigestTipAddress"))

instance Hashable GetDigest where

instance NFData GetDigest where

instance ToHeaders GetDigest where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON GetDigest where
        toJSON = const (Object mempty)

instance ToPath GetDigest where
        toPath GetDigest'{..}
          = mconcat ["/ledgers/", toBS _gdName, "/digest"]

instance ToQuery GetDigest where
        toQuery = const mempty

-- | /See:/ 'getDigestResponse' smart constructor.
data GetDigestResponse = GetDigestResponse'
  { _gdrsResponseStatus   :: !Int
  , _gdrsDigest           :: !Base64
  , _gdrsDigestTipAddress :: !(Sensitive ValueHolder)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDigestResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsResponseStatus' - -- | The response status code.
--
-- * 'gdrsDigest' - Undocumented member.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'gdrsDigestTipAddress' - Undocumented member.
getDigestResponse
    :: Int -- ^ 'gdrsResponseStatus'
    -> ByteString -- ^ 'gdrsDigest'
    -> ValueHolder -- ^ 'gdrsDigestTipAddress'
    -> GetDigestResponse
getDigestResponse pResponseStatus_ pDigest_ pDigestTipAddress_ =
  GetDigestResponse'
    { _gdrsResponseStatus = pResponseStatus_
    , _gdrsDigest = _Base64 # pDigest_
    , _gdrsDigestTipAddress = _Sensitive # pDigestTipAddress_
    }


-- | -- | The response status code.
gdrsResponseStatus :: Lens' GetDigestResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\ s a -> s{_gdrsResponseStatus = a})

-- | Undocumented member.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
gdrsDigest :: Lens' GetDigestResponse ByteString
gdrsDigest = lens _gdrsDigest (\ s a -> s{_gdrsDigest = a}) . _Base64

-- | Undocumented member.
gdrsDigestTipAddress :: Lens' GetDigestResponse ValueHolder
gdrsDigestTipAddress = lens _gdrsDigestTipAddress (\ s a -> s{_gdrsDigestTipAddress = a}) . _Sensitive

instance NFData GetDigestResponse where

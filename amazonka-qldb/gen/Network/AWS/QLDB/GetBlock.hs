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
-- Module      : Network.AWS.QLDB.GetBlock
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.QLDB.GetBlock
    (
    -- * Creating a Request
      getBlock
    , GetBlock
    -- * Request Lenses
    , gbDigestTipAddress
    , gbName
    , gbBlockAddress

    -- * Destructuring the Response
    , getBlockResponse
    , GetBlockResponse
    -- * Response Lenses
    , gbrsProof
    , gbrsResponseStatus
    , gbrsBlock
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.QLDB.Types
import Network.AWS.QLDB.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBlock' smart constructor.
data GetBlock = GetBlock'
  { _gbDigestTipAddress :: !(Maybe (Sensitive ValueHolder))
  , _gbName             :: !Text
  , _gbBlockAddress     :: !(Sensitive ValueHolder)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBlock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbDigestTipAddress' - Undocumented member.
--
-- * 'gbName' - Undocumented member.
--
-- * 'gbBlockAddress' - Undocumented member.
getBlock
    :: Text -- ^ 'gbName'
    -> ValueHolder -- ^ 'gbBlockAddress'
    -> GetBlock
getBlock pName_ pBlockAddress_ =
  GetBlock'
    { _gbDigestTipAddress = Nothing
    , _gbName = pName_
    , _gbBlockAddress = _Sensitive # pBlockAddress_
    }


-- | Undocumented member.
gbDigestTipAddress :: Lens' GetBlock (Maybe ValueHolder)
gbDigestTipAddress = lens _gbDigestTipAddress (\ s a -> s{_gbDigestTipAddress = a}) . mapping _Sensitive

-- | Undocumented member.
gbName :: Lens' GetBlock Text
gbName = lens _gbName (\ s a -> s{_gbName = a})

-- | Undocumented member.
gbBlockAddress :: Lens' GetBlock ValueHolder
gbBlockAddress = lens _gbBlockAddress (\ s a -> s{_gbBlockAddress = a}) . _Sensitive

instance AWSRequest GetBlock where
        type Rs GetBlock = GetBlockResponse
        request = postJSON qldb
        response
          = receiveJSON
              (\ s h x ->
                 GetBlockResponse' <$>
                   (x .?> "Proof") <*> (pure (fromEnum s)) <*>
                     (x .:> "Block"))

instance Hashable GetBlock where

instance NFData GetBlock where

instance ToHeaders GetBlock where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON GetBlock where
        toJSON GetBlock'{..}
          = object
              (catMaybes
                 [("DigestTipAddress" .=) <$> _gbDigestTipAddress,
                  Just ("BlockAddress" .= _gbBlockAddress)])

instance ToPath GetBlock where
        toPath GetBlock'{..}
          = mconcat ["/ledgers/", toBS _gbName, "/block"]

instance ToQuery GetBlock where
        toQuery = const mempty

-- | /See:/ 'getBlockResponse' smart constructor.
data GetBlockResponse = GetBlockResponse'
  { _gbrsProof          :: !(Maybe (Sensitive ValueHolder))
  , _gbrsResponseStatus :: !Int
  , _gbrsBlock          :: !(Sensitive ValueHolder)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBlockResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbrsProof' - Undocumented member.
--
-- * 'gbrsResponseStatus' - -- | The response status code.
--
-- * 'gbrsBlock' - Undocumented member.
getBlockResponse
    :: Int -- ^ 'gbrsResponseStatus'
    -> ValueHolder -- ^ 'gbrsBlock'
    -> GetBlockResponse
getBlockResponse pResponseStatus_ pBlock_ =
  GetBlockResponse'
    { _gbrsProof = Nothing
    , _gbrsResponseStatus = pResponseStatus_
    , _gbrsBlock = _Sensitive # pBlock_
    }


-- | Undocumented member.
gbrsProof :: Lens' GetBlockResponse (Maybe ValueHolder)
gbrsProof = lens _gbrsProof (\ s a -> s{_gbrsProof = a}) . mapping _Sensitive

-- | -- | The response status code.
gbrsResponseStatus :: Lens' GetBlockResponse Int
gbrsResponseStatus = lens _gbrsResponseStatus (\ s a -> s{_gbrsResponseStatus = a})

-- | Undocumented member.
gbrsBlock :: Lens' GetBlockResponse ValueHolder
gbrsBlock = lens _gbrsBlock (\ s a -> s{_gbrsBlock = a}) . _Sensitive

instance NFData GetBlockResponse where

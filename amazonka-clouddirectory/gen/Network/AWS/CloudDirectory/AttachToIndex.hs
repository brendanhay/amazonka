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
-- Module      : Network.AWS.CloudDirectory.AttachToIndex
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified object to the specified index.
--
--
module Network.AWS.CloudDirectory.AttachToIndex
    (
    -- * Creating a Request
      attachToIndex
    , AttachToIndex
    -- * Request Lenses
    , atiDirectoryARN
    , atiIndexReference
    , atiTargetReference

    -- * Destructuring the Response
    , attachToIndexResponse
    , AttachToIndexResponse
    -- * Response Lenses
    , atirsAttachedObjectIdentifier
    , atirsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'attachToIndex' smart constructor.
data AttachToIndex = AttachToIndex'
  { _atiDirectoryARN    :: !Text
  , _atiIndexReference  :: !ObjectReference
  , _atiTargetReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachToIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atiDirectoryARN' - The Amazon Resource Name (ARN) of the directory where the object and index exist.
--
-- * 'atiIndexReference' - A reference to the index that you are attaching the object to.
--
-- * 'atiTargetReference' - A reference to the object that you are attaching to the index.
attachToIndex
    :: Text -- ^ 'atiDirectoryARN'
    -> ObjectReference -- ^ 'atiIndexReference'
    -> ObjectReference -- ^ 'atiTargetReference'
    -> AttachToIndex
attachToIndex pDirectoryARN_ pIndexReference_ pTargetReference_ =
  AttachToIndex'
    { _atiDirectoryARN = pDirectoryARN_
    , _atiIndexReference = pIndexReference_
    , _atiTargetReference = pTargetReference_
    }


-- | The Amazon Resource Name (ARN) of the directory where the object and index exist.
atiDirectoryARN :: Lens' AttachToIndex Text
atiDirectoryARN = lens _atiDirectoryARN (\ s a -> s{_atiDirectoryARN = a})

-- | A reference to the index that you are attaching the object to.
atiIndexReference :: Lens' AttachToIndex ObjectReference
atiIndexReference = lens _atiIndexReference (\ s a -> s{_atiIndexReference = a})

-- | A reference to the object that you are attaching to the index.
atiTargetReference :: Lens' AttachToIndex ObjectReference
atiTargetReference = lens _atiTargetReference (\ s a -> s{_atiTargetReference = a})

instance AWSRequest AttachToIndex where
        type Rs AttachToIndex = AttachToIndexResponse
        request = putJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 AttachToIndexResponse' <$>
                   (x .?> "AttachedObjectIdentifier") <*>
                     (pure (fromEnum s)))

instance Hashable AttachToIndex where

instance NFData AttachToIndex where

instance ToHeaders AttachToIndex where
        toHeaders AttachToIndex'{..}
          = mconcat
              ["x-amz-data-partition" =# _atiDirectoryARN]

instance ToJSON AttachToIndex where
        toJSON AttachToIndex'{..}
          = object
              (catMaybes
                 [Just ("IndexReference" .= _atiIndexReference),
                  Just ("TargetReference" .= _atiTargetReference)])

instance ToPath AttachToIndex where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/index/attach"

instance ToQuery AttachToIndex where
        toQuery = const mempty

-- | /See:/ 'attachToIndexResponse' smart constructor.
data AttachToIndexResponse = AttachToIndexResponse'
  { _atirsAttachedObjectIdentifier :: !(Maybe Text)
  , _atirsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachToIndexResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atirsAttachedObjectIdentifier' - The @ObjectIdentifier@ of the object that was attached to the index.
--
-- * 'atirsResponseStatus' - -- | The response status code.
attachToIndexResponse
    :: Int -- ^ 'atirsResponseStatus'
    -> AttachToIndexResponse
attachToIndexResponse pResponseStatus_ =
  AttachToIndexResponse'
    { _atirsAttachedObjectIdentifier = Nothing
    , _atirsResponseStatus = pResponseStatus_
    }


-- | The @ObjectIdentifier@ of the object that was attached to the index.
atirsAttachedObjectIdentifier :: Lens' AttachToIndexResponse (Maybe Text)
atirsAttachedObjectIdentifier = lens _atirsAttachedObjectIdentifier (\ s a -> s{_atirsAttachedObjectIdentifier = a})

-- | -- | The response status code.
atirsResponseStatus :: Lens' AttachToIndexResponse Int
atirsResponseStatus = lens _atirsResponseStatus (\ s a -> s{_atirsResponseStatus = a})

instance NFData AttachToIndexResponse where

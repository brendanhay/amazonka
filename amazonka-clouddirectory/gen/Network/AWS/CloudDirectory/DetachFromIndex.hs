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
-- Module      : Network.AWS.CloudDirectory.DetachFromIndex
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches the specified object from the specified index.
--
--
module Network.AWS.CloudDirectory.DetachFromIndex
    (
    -- * Creating a Request
      detachFromIndex
    , DetachFromIndex
    -- * Request Lenses
    , dfiDirectoryARN
    , dfiIndexReference
    , dfiTargetReference

    -- * Destructuring the Response
    , detachFromIndexResponse
    , DetachFromIndexResponse
    -- * Response Lenses
    , dfirsDetachedObjectIdentifier
    , dfirsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachFromIndex' smart constructor.
data DetachFromIndex = DetachFromIndex'
  { _dfiDirectoryARN    :: !Text
  , _dfiIndexReference  :: !ObjectReference
  , _dfiTargetReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachFromIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfiDirectoryARN' - The Amazon Resource Name (ARN) of the directory the index and object exist in.
--
-- * 'dfiIndexReference' - A reference to the index object.
--
-- * 'dfiTargetReference' - A reference to the object being detached from the index.
detachFromIndex
    :: Text -- ^ 'dfiDirectoryARN'
    -> ObjectReference -- ^ 'dfiIndexReference'
    -> ObjectReference -- ^ 'dfiTargetReference'
    -> DetachFromIndex
detachFromIndex pDirectoryARN_ pIndexReference_ pTargetReference_ =
  DetachFromIndex'
    { _dfiDirectoryARN = pDirectoryARN_
    , _dfiIndexReference = pIndexReference_
    , _dfiTargetReference = pTargetReference_
    }


-- | The Amazon Resource Name (ARN) of the directory the index and object exist in.
dfiDirectoryARN :: Lens' DetachFromIndex Text
dfiDirectoryARN = lens _dfiDirectoryARN (\ s a -> s{_dfiDirectoryARN = a})

-- | A reference to the index object.
dfiIndexReference :: Lens' DetachFromIndex ObjectReference
dfiIndexReference = lens _dfiIndexReference (\ s a -> s{_dfiIndexReference = a})

-- | A reference to the object being detached from the index.
dfiTargetReference :: Lens' DetachFromIndex ObjectReference
dfiTargetReference = lens _dfiTargetReference (\ s a -> s{_dfiTargetReference = a})

instance AWSRequest DetachFromIndex where
        type Rs DetachFromIndex = DetachFromIndexResponse
        request = putJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 DetachFromIndexResponse' <$>
                   (x .?> "DetachedObjectIdentifier") <*>
                     (pure (fromEnum s)))

instance Hashable DetachFromIndex where

instance NFData DetachFromIndex where

instance ToHeaders DetachFromIndex where
        toHeaders DetachFromIndex'{..}
          = mconcat
              ["x-amz-data-partition" =# _dfiDirectoryARN]

instance ToJSON DetachFromIndex where
        toJSON DetachFromIndex'{..}
          = object
              (catMaybes
                 [Just ("IndexReference" .= _dfiIndexReference),
                  Just ("TargetReference" .= _dfiTargetReference)])

instance ToPath DetachFromIndex where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/index/detach"

instance ToQuery DetachFromIndex where
        toQuery = const mempty

-- | /See:/ 'detachFromIndexResponse' smart constructor.
data DetachFromIndexResponse = DetachFromIndexResponse'
  { _dfirsDetachedObjectIdentifier :: !(Maybe Text)
  , _dfirsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachFromIndexResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfirsDetachedObjectIdentifier' - The @ObjectIdentifier@ of the object that was detached from the index.
--
-- * 'dfirsResponseStatus' - -- | The response status code.
detachFromIndexResponse
    :: Int -- ^ 'dfirsResponseStatus'
    -> DetachFromIndexResponse
detachFromIndexResponse pResponseStatus_ =
  DetachFromIndexResponse'
    { _dfirsDetachedObjectIdentifier = Nothing
    , _dfirsResponseStatus = pResponseStatus_
    }


-- | The @ObjectIdentifier@ of the object that was detached from the index.
dfirsDetachedObjectIdentifier :: Lens' DetachFromIndexResponse (Maybe Text)
dfirsDetachedObjectIdentifier = lens _dfirsDetachedObjectIdentifier (\ s a -> s{_dfirsDetachedObjectIdentifier = a})

-- | -- | The response status code.
dfirsResponseStatus :: Lens' DetachFromIndexResponse Int
dfirsResponseStatus = lens _dfirsResponseStatus (\ s a -> s{_dfirsResponseStatus = a})

instance NFData DetachFromIndexResponse where

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
-- Module      : Network.AWS.CloudDirectory.DetachObject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a given object from the parent object. The object that is to be detached from the parent is specified by the link name.
--
--
module Network.AWS.CloudDirectory.DetachObject
    (
    -- * Creating a Request
      detachObject
    , DetachObject
    -- * Request Lenses
    , detDirectoryARN
    , detParentReference
    , detLinkName

    -- * Destructuring the Response
    , detachObjectResponse
    , DetachObjectResponse
    -- * Response Lenses
    , detrsDetachedObjectIdentifier
    , detrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachObject' smart constructor.
data DetachObject = DetachObject'
  { _detDirectoryARN    :: !Text
  , _detParentReference :: !ObjectReference
  , _detLinkName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
--
-- * 'detParentReference' - The parent reference from which the object with the specified link name is detached.
--
-- * 'detLinkName' - The link name associated with the object that needs to be detached.
detachObject
    :: Text -- ^ 'detDirectoryARN'
    -> ObjectReference -- ^ 'detParentReference'
    -> Text -- ^ 'detLinkName'
    -> DetachObject
detachObject pDirectoryARN_ pParentReference_ pLinkName_ =
  DetachObject'
    { _detDirectoryARN = pDirectoryARN_
    , _detParentReference = pParentReference_
    , _detLinkName = pLinkName_
    }


-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
detDirectoryARN :: Lens' DetachObject Text
detDirectoryARN = lens _detDirectoryARN (\ s a -> s{_detDirectoryARN = a})

-- | The parent reference from which the object with the specified link name is detached.
detParentReference :: Lens' DetachObject ObjectReference
detParentReference = lens _detParentReference (\ s a -> s{_detParentReference = a})

-- | The link name associated with the object that needs to be detached.
detLinkName :: Lens' DetachObject Text
detLinkName = lens _detLinkName (\ s a -> s{_detLinkName = a})

instance AWSRequest DetachObject where
        type Rs DetachObject = DetachObjectResponse
        request = putJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 DetachObjectResponse' <$>
                   (x .?> "DetachedObjectIdentifier") <*>
                     (pure (fromEnum s)))

instance Hashable DetachObject where

instance NFData DetachObject where

instance ToHeaders DetachObject where
        toHeaders DetachObject'{..}
          = mconcat
              ["x-amz-data-partition" =# _detDirectoryARN]

instance ToJSON DetachObject where
        toJSON DetachObject'{..}
          = object
              (catMaybes
                 [Just ("ParentReference" .= _detParentReference),
                  Just ("LinkName" .= _detLinkName)])

instance ToPath DetachObject where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/object/detach"

instance ToQuery DetachObject where
        toQuery = const mempty

-- | /See:/ 'detachObjectResponse' smart constructor.
data DetachObjectResponse = DetachObjectResponse'
  { _detrsDetachedObjectIdentifier :: !(Maybe Text)
  , _detrsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detrsDetachedObjectIdentifier' - The @ObjectIdentifier@ that was detached from the object.
--
-- * 'detrsResponseStatus' - -- | The response status code.
detachObjectResponse
    :: Int -- ^ 'detrsResponseStatus'
    -> DetachObjectResponse
detachObjectResponse pResponseStatus_ =
  DetachObjectResponse'
    { _detrsDetachedObjectIdentifier = Nothing
    , _detrsResponseStatus = pResponseStatus_
    }


-- | The @ObjectIdentifier@ that was detached from the object.
detrsDetachedObjectIdentifier :: Lens' DetachObjectResponse (Maybe Text)
detrsDetachedObjectIdentifier = lens _detrsDetachedObjectIdentifier (\ s a -> s{_detrsDetachedObjectIdentifier = a})

-- | -- | The response status code.
detrsResponseStatus :: Lens' DetachObjectResponse Int
detrsResponseStatus = lens _detrsResponseStatus (\ s a -> s{_detrsResponseStatus = a})

instance NFData DetachObjectResponse where

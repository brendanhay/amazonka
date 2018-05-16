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
-- Module      : Network.AWS.CloudDirectory.AttachObject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an existing object to another object. An object can be accessed in two ways:
--
--
--     * Using the path
--
--     * Using @ObjectIdentifier@
--
--
--
module Network.AWS.CloudDirectory.AttachObject
    (
    -- * Creating a Request
      attachObject
    , AttachObject
    -- * Request Lenses
    , aoDirectoryARN
    , aoParentReference
    , aoChildReference
    , aoLinkName

    -- * Destructuring the Response
    , attachObjectResponse
    , AttachObjectResponse
    -- * Response Lenses
    , aorsAttachedObjectIdentifier
    , aorsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'attachObject' smart constructor.
data AttachObject = AttachObject'
  { _aoDirectoryARN    :: !Text
  , _aoParentReference :: !ObjectReference
  , _aoChildReference  :: !ObjectReference
  , _aoLinkName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aoDirectoryARN' - Amazon Resource Name (ARN) that is associated with the 'Directory' where both objects reside. For more information, see 'arns' .
--
-- * 'aoParentReference' - The parent object reference.
--
-- * 'aoChildReference' - The child object reference to be attached to the object.
--
-- * 'aoLinkName' - The link name with which the child object is attached to the parent.
attachObject
    :: Text -- ^ 'aoDirectoryARN'
    -> ObjectReference -- ^ 'aoParentReference'
    -> ObjectReference -- ^ 'aoChildReference'
    -> Text -- ^ 'aoLinkName'
    -> AttachObject
attachObject pDirectoryARN_ pParentReference_ pChildReference_ pLinkName_ =
  AttachObject'
    { _aoDirectoryARN = pDirectoryARN_
    , _aoParentReference = pParentReference_
    , _aoChildReference = pChildReference_
    , _aoLinkName = pLinkName_
    }


-- | Amazon Resource Name (ARN) that is associated with the 'Directory' where both objects reside. For more information, see 'arns' .
aoDirectoryARN :: Lens' AttachObject Text
aoDirectoryARN = lens _aoDirectoryARN (\ s a -> s{_aoDirectoryARN = a})

-- | The parent object reference.
aoParentReference :: Lens' AttachObject ObjectReference
aoParentReference = lens _aoParentReference (\ s a -> s{_aoParentReference = a})

-- | The child object reference to be attached to the object.
aoChildReference :: Lens' AttachObject ObjectReference
aoChildReference = lens _aoChildReference (\ s a -> s{_aoChildReference = a})

-- | The link name with which the child object is attached to the parent.
aoLinkName :: Lens' AttachObject Text
aoLinkName = lens _aoLinkName (\ s a -> s{_aoLinkName = a})

instance AWSRequest AttachObject where
        type Rs AttachObject = AttachObjectResponse
        request = putJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 AttachObjectResponse' <$>
                   (x .?> "AttachedObjectIdentifier") <*>
                     (pure (fromEnum s)))

instance Hashable AttachObject where

instance NFData AttachObject where

instance ToHeaders AttachObject where
        toHeaders AttachObject'{..}
          = mconcat ["x-amz-data-partition" =# _aoDirectoryARN]

instance ToJSON AttachObject where
        toJSON AttachObject'{..}
          = object
              (catMaybes
                 [Just ("ParentReference" .= _aoParentReference),
                  Just ("ChildReference" .= _aoChildReference),
                  Just ("LinkName" .= _aoLinkName)])

instance ToPath AttachObject where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/object/attach"

instance ToQuery AttachObject where
        toQuery = const mempty

-- | /See:/ 'attachObjectResponse' smart constructor.
data AttachObjectResponse = AttachObjectResponse'
  { _aorsAttachedObjectIdentifier :: !(Maybe Text)
  , _aorsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aorsAttachedObjectIdentifier' - The attached @ObjectIdentifier@ , which is the child @ObjectIdentifier@ .
--
-- * 'aorsResponseStatus' - -- | The response status code.
attachObjectResponse
    :: Int -- ^ 'aorsResponseStatus'
    -> AttachObjectResponse
attachObjectResponse pResponseStatus_ =
  AttachObjectResponse'
    { _aorsAttachedObjectIdentifier = Nothing
    , _aorsResponseStatus = pResponseStatus_
    }


-- | The attached @ObjectIdentifier@ , which is the child @ObjectIdentifier@ .
aorsAttachedObjectIdentifier :: Lens' AttachObjectResponse (Maybe Text)
aorsAttachedObjectIdentifier = lens _aorsAttachedObjectIdentifier (\ s a -> s{_aorsAttachedObjectIdentifier = a})

-- | -- | The response status code.
aorsResponseStatus :: Lens' AttachObjectResponse Int
aorsResponseStatus = lens _aorsResponseStatus (\ s a -> s{_aorsResponseStatus = a})

instance NFData AttachObjectResponse where

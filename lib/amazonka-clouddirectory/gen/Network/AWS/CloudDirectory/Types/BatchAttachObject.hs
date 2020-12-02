{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachObject where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of an 'AttachObject' operation.
--
--
--
-- /See:/ 'batchAttachObject' smart constructor.
data BatchAttachObject = BatchAttachObject'
  { _baoParentReference ::
      !ObjectReference,
    _baoChildReference :: !ObjectReference,
    _baoLinkName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchAttachObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'baoParentReference' - The parent object reference.
--
-- * 'baoChildReference' - The child object reference that is to be attached to the object.
--
-- * 'baoLinkName' - The name of the link.
batchAttachObject ::
  -- | 'baoParentReference'
  ObjectReference ->
  -- | 'baoChildReference'
  ObjectReference ->
  -- | 'baoLinkName'
  Text ->
  BatchAttachObject
batchAttachObject pParentReference_ pChildReference_ pLinkName_ =
  BatchAttachObject'
    { _baoParentReference = pParentReference_,
      _baoChildReference = pChildReference_,
      _baoLinkName = pLinkName_
    }

-- | The parent object reference.
baoParentReference :: Lens' BatchAttachObject ObjectReference
baoParentReference = lens _baoParentReference (\s a -> s {_baoParentReference = a})

-- | The child object reference that is to be attached to the object.
baoChildReference :: Lens' BatchAttachObject ObjectReference
baoChildReference = lens _baoChildReference (\s a -> s {_baoChildReference = a})

-- | The name of the link.
baoLinkName :: Lens' BatchAttachObject Text
baoLinkName = lens _baoLinkName (\s a -> s {_baoLinkName = a})

instance Hashable BatchAttachObject

instance NFData BatchAttachObject

instance ToJSON BatchAttachObject where
  toJSON BatchAttachObject' {..} =
    object
      ( catMaybes
          [ Just ("ParentReference" .= _baoParentReference),
            Just ("ChildReference" .= _baoChildReference),
            Just ("LinkName" .= _baoLinkName)
          ]
      )

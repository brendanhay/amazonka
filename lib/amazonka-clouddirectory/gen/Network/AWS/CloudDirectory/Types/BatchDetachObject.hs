{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachObject where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'DetachObject' operation.
--
--
--
-- /See:/ 'batchDetachObject' smart constructor.
data BatchDetachObject = BatchDetachObject'
  { _bdoBatchReferenceName ::
      !(Maybe Text),
    _bdoParentReference :: !ObjectReference,
    _bdoLinkName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDetachObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdoBatchReferenceName' - The batch reference name. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support> for more information.
--
-- * 'bdoParentReference' - Parent reference from which the object with the specified link name is detached.
--
-- * 'bdoLinkName' - The name of the link.
batchDetachObject ::
  -- | 'bdoParentReference'
  ObjectReference ->
  -- | 'bdoLinkName'
  Text ->
  BatchDetachObject
batchDetachObject pParentReference_ pLinkName_ =
  BatchDetachObject'
    { _bdoBatchReferenceName = Nothing,
      _bdoParentReference = pParentReference_,
      _bdoLinkName = pLinkName_
    }

-- | The batch reference name. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support> for more information.
bdoBatchReferenceName :: Lens' BatchDetachObject (Maybe Text)
bdoBatchReferenceName = lens _bdoBatchReferenceName (\s a -> s {_bdoBatchReferenceName = a})

-- | Parent reference from which the object with the specified link name is detached.
bdoParentReference :: Lens' BatchDetachObject ObjectReference
bdoParentReference = lens _bdoParentReference (\s a -> s {_bdoParentReference = a})

-- | The name of the link.
bdoLinkName :: Lens' BatchDetachObject Text
bdoLinkName = lens _bdoLinkName (\s a -> s {_bdoLinkName = a})

instance Hashable BatchDetachObject

instance NFData BatchDetachObject

instance ToJSON BatchDetachObject where
  toJSON BatchDetachObject' {..} =
    object
      ( catMaybes
          [ ("BatchReferenceName" .=) <$> _bdoBatchReferenceName,
            Just ("ParentReference" .= _bdoParentReference),
            Just ("LinkName" .= _bdoLinkName)
          ]
      )

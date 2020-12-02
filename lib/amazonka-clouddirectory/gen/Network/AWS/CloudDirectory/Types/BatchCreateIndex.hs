{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchCreateIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchCreateIndex where

import Network.AWS.CloudDirectory.Types.AttributeKey
import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Creates an index object inside of a 'BatchRead' operation. For more information, see 'CreateIndex' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchCreateIndex' smart constructor.
data BatchCreateIndex = BatchCreateIndex'
  { _bciParentReference ::
      !(Maybe ObjectReference),
    _bciLinkName :: !(Maybe Text),
    _bciBatchReferenceName :: !(Maybe Text),
    _bciOrderedIndexedAttributeList :: ![AttributeKey],
    _bciIsUnique :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchCreateIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bciParentReference' - A reference to the parent object that contains the index object.
--
-- * 'bciLinkName' - The name of the link between the parent object and the index object.
--
-- * 'bciBatchReferenceName' - The batch reference name. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support> for more information.
--
-- * 'bciOrderedIndexedAttributeList' - Specifies the attributes that should be indexed on. Currently only a single attribute is supported.
--
-- * 'bciIsUnique' - Indicates whether the attribute that is being indexed has unique values or not.
batchCreateIndex ::
  -- | 'bciIsUnique'
  Bool ->
  BatchCreateIndex
batchCreateIndex pIsUnique_ =
  BatchCreateIndex'
    { _bciParentReference = Nothing,
      _bciLinkName = Nothing,
      _bciBatchReferenceName = Nothing,
      _bciOrderedIndexedAttributeList = mempty,
      _bciIsUnique = pIsUnique_
    }

-- | A reference to the parent object that contains the index object.
bciParentReference :: Lens' BatchCreateIndex (Maybe ObjectReference)
bciParentReference = lens _bciParentReference (\s a -> s {_bciParentReference = a})

-- | The name of the link between the parent object and the index object.
bciLinkName :: Lens' BatchCreateIndex (Maybe Text)
bciLinkName = lens _bciLinkName (\s a -> s {_bciLinkName = a})

-- | The batch reference name. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support> for more information.
bciBatchReferenceName :: Lens' BatchCreateIndex (Maybe Text)
bciBatchReferenceName = lens _bciBatchReferenceName (\s a -> s {_bciBatchReferenceName = a})

-- | Specifies the attributes that should be indexed on. Currently only a single attribute is supported.
bciOrderedIndexedAttributeList :: Lens' BatchCreateIndex [AttributeKey]
bciOrderedIndexedAttributeList = lens _bciOrderedIndexedAttributeList (\s a -> s {_bciOrderedIndexedAttributeList = a}) . _Coerce

-- | Indicates whether the attribute that is being indexed has unique values or not.
bciIsUnique :: Lens' BatchCreateIndex Bool
bciIsUnique = lens _bciIsUnique (\s a -> s {_bciIsUnique = a})

instance Hashable BatchCreateIndex

instance NFData BatchCreateIndex

instance ToJSON BatchCreateIndex where
  toJSON BatchCreateIndex' {..} =
    object
      ( catMaybes
          [ ("ParentReference" .=) <$> _bciParentReference,
            ("LinkName" .=) <$> _bciLinkName,
            ("BatchReferenceName" .=) <$> _bciBatchReferenceName,
            Just
              ("OrderedIndexedAttributeList" .= _bciOrderedIndexedAttributeList),
            Just ("IsUnique" .= _bciIsUnique)
          ]
      )

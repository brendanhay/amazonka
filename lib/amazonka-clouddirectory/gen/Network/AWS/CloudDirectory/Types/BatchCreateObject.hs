{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchCreateObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchCreateObject where

import Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.SchemaFacet
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'CreateObject' operation.
--
--
--
-- /See:/ 'batchCreateObject' smart constructor.
data BatchCreateObject = BatchCreateObject'
  { _bcoParentReference ::
      !(Maybe ObjectReference),
    _bcoLinkName :: !(Maybe Text),
    _bcoBatchReferenceName :: !(Maybe Text),
    _bcoSchemaFacet :: ![SchemaFacet],
    _bcoObjectAttributeList :: ![AttributeKeyAndValue]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchCreateObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bcoParentReference' - If specified, the parent reference to which this object will be attached.
--
-- * 'bcoLinkName' - The name of the link.
--
-- * 'bcoBatchReferenceName' - The batch reference name. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support> for more information.
--
-- * 'bcoSchemaFacet' - A list of @FacetArns@ that will be associated with the object. For more information, see 'arns' .
--
-- * 'bcoObjectAttributeList' - An attribute map, which contains an attribute ARN as the key and attribute value as the map value.
batchCreateObject ::
  BatchCreateObject
batchCreateObject =
  BatchCreateObject'
    { _bcoParentReference = Nothing,
      _bcoLinkName = Nothing,
      _bcoBatchReferenceName = Nothing,
      _bcoSchemaFacet = mempty,
      _bcoObjectAttributeList = mempty
    }

-- | If specified, the parent reference to which this object will be attached.
bcoParentReference :: Lens' BatchCreateObject (Maybe ObjectReference)
bcoParentReference = lens _bcoParentReference (\s a -> s {_bcoParentReference = a})

-- | The name of the link.
bcoLinkName :: Lens' BatchCreateObject (Maybe Text)
bcoLinkName = lens _bcoLinkName (\s a -> s {_bcoLinkName = a})

-- | The batch reference name. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support> for more information.
bcoBatchReferenceName :: Lens' BatchCreateObject (Maybe Text)
bcoBatchReferenceName = lens _bcoBatchReferenceName (\s a -> s {_bcoBatchReferenceName = a})

-- | A list of @FacetArns@ that will be associated with the object. For more information, see 'arns' .
bcoSchemaFacet :: Lens' BatchCreateObject [SchemaFacet]
bcoSchemaFacet = lens _bcoSchemaFacet (\s a -> s {_bcoSchemaFacet = a}) . _Coerce

-- | An attribute map, which contains an attribute ARN as the key and attribute value as the map value.
bcoObjectAttributeList :: Lens' BatchCreateObject [AttributeKeyAndValue]
bcoObjectAttributeList = lens _bcoObjectAttributeList (\s a -> s {_bcoObjectAttributeList = a}) . _Coerce

instance Hashable BatchCreateObject

instance NFData BatchCreateObject

instance ToJSON BatchCreateObject where
  toJSON BatchCreateObject' {..} =
    object
      ( catMaybes
          [ ("ParentReference" .=) <$> _bcoParentReference,
            ("LinkName" .=) <$> _bcoLinkName,
            ("BatchReferenceName" .=) <$> _bcoBatchReferenceName,
            Just ("SchemaFacet" .= _bcoSchemaFacet),
            Just ("ObjectAttributeList" .= _bcoObjectAttributeList)
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchGetObjectInformationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchGetObjectInformationResponse where

import Network.AWS.CloudDirectory.Types.SchemaFacet
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'GetObjectInformation' response operation.
--
--
--
-- /See:/ 'batchGetObjectInformationResponse' smart constructor.
data BatchGetObjectInformationResponse = BatchGetObjectInformationResponse'
  { _bgoiObjectIdentifier ::
      !(Maybe Text),
    _bgoiSchemaFacets ::
      !(Maybe [SchemaFacet])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetObjectInformationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgoiObjectIdentifier' - The @ObjectIdentifier@ of the specified object.
--
-- * 'bgoiSchemaFacets' - The facets attached to the specified object.
batchGetObjectInformationResponse ::
  BatchGetObjectInformationResponse
batchGetObjectInformationResponse =
  BatchGetObjectInformationResponse'
    { _bgoiObjectIdentifier =
        Nothing,
      _bgoiSchemaFacets = Nothing
    }

-- | The @ObjectIdentifier@ of the specified object.
bgoiObjectIdentifier :: Lens' BatchGetObjectInformationResponse (Maybe Text)
bgoiObjectIdentifier = lens _bgoiObjectIdentifier (\s a -> s {_bgoiObjectIdentifier = a})

-- | The facets attached to the specified object.
bgoiSchemaFacets :: Lens' BatchGetObjectInformationResponse [SchemaFacet]
bgoiSchemaFacets = lens _bgoiSchemaFacets (\s a -> s {_bgoiSchemaFacets = a}) . _Default . _Coerce

instance FromJSON BatchGetObjectInformationResponse where
  parseJSON =
    withObject
      "BatchGetObjectInformationResponse"
      ( \x ->
          BatchGetObjectInformationResponse'
            <$> (x .:? "ObjectIdentifier") <*> (x .:? "SchemaFacets" .!= mempty)
      )

instance Hashable BatchGetObjectInformationResponse

instance NFData BatchGetObjectInformationResponse

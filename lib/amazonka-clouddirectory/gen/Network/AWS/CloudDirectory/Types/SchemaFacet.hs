{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.SchemaFacet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.SchemaFacet where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A facet.
--
--
--
-- /See:/ 'schemaFacet' smart constructor.
data SchemaFacet = SchemaFacet'
  { _sfFacetName :: !(Maybe Text),
    _sfSchemaARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SchemaFacet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfFacetName' - The name of the facet.
--
-- * 'sfSchemaARN' - The ARN of the schema that contains the facet with no minor component. See 'arns' and <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_inplaceschemaupgrade.html In-Place Schema Upgrade> for a description of when to provide minor versions.
schemaFacet ::
  SchemaFacet
schemaFacet =
  SchemaFacet' {_sfFacetName = Nothing, _sfSchemaARN = Nothing}

-- | The name of the facet.
sfFacetName :: Lens' SchemaFacet (Maybe Text)
sfFacetName = lens _sfFacetName (\s a -> s {_sfFacetName = a})

-- | The ARN of the schema that contains the facet with no minor component. See 'arns' and <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_inplaceschemaupgrade.html In-Place Schema Upgrade> for a description of when to provide minor versions.
sfSchemaARN :: Lens' SchemaFacet (Maybe Text)
sfSchemaARN = lens _sfSchemaARN (\s a -> s {_sfSchemaARN = a})

instance FromJSON SchemaFacet where
  parseJSON =
    withObject
      "SchemaFacet"
      ( \x ->
          SchemaFacet' <$> (x .:? "FacetName") <*> (x .:? "SchemaArn")
      )

instance Hashable SchemaFacet

instance NFData SchemaFacet

instance ToJSON SchemaFacet where
  toJSON SchemaFacet' {..} =
    object
      ( catMaybes
          [ ("FacetName" .=) <$> _sfFacetName,
            ("SchemaArn" .=) <$> _sfSchemaARN
          ]
      )

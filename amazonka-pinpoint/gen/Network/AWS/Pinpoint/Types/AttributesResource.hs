{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.AttributesResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.AttributesResource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the type and the names of attributes that
-- were removed from all the endpoints that are associated with an
-- application.
--
-- /See:/ 'newAttributesResource' smart constructor.
data AttributesResource = AttributesResource'
  { -- | An array that specifies the names of the attributes that were removed
    -- from the endpoints.
    attributes :: Prelude.Maybe [Prelude.Text],
    -- | The type of attribute or attributes that were removed from the
    -- endpoints. Valid values are:
    --
    -- -   endpoint-custom-attributes - Custom attributes that describe
    --     endpoints.
    --
    -- -   endpoint-metric-attributes - Custom metrics that your app reports to
    --     Amazon Pinpoint for endpoints.
    --
    -- -   endpoint-user-attributes - Custom attributes that describe users.
    attributeType :: Prelude.Text,
    -- | The unique identifier for the application.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AttributesResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'attributesResource_attributes' - An array that specifies the names of the attributes that were removed
-- from the endpoints.
--
-- 'attributeType', 'attributesResource_attributeType' - The type of attribute or attributes that were removed from the
-- endpoints. Valid values are:
--
-- -   endpoint-custom-attributes - Custom attributes that describe
--     endpoints.
--
-- -   endpoint-metric-attributes - Custom metrics that your app reports to
--     Amazon Pinpoint for endpoints.
--
-- -   endpoint-user-attributes - Custom attributes that describe users.
--
-- 'applicationId', 'attributesResource_applicationId' - The unique identifier for the application.
newAttributesResource ::
  -- | 'attributeType'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  AttributesResource
newAttributesResource pAttributeType_ pApplicationId_ =
  AttributesResource'
    { attributes = Prelude.Nothing,
      attributeType = pAttributeType_,
      applicationId = pApplicationId_
    }

-- | An array that specifies the names of the attributes that were removed
-- from the endpoints.
attributesResource_attributes :: Lens.Lens' AttributesResource (Prelude.Maybe [Prelude.Text])
attributesResource_attributes = Lens.lens (\AttributesResource' {attributes} -> attributes) (\s@AttributesResource' {} a -> s {attributes = a} :: AttributesResource) Prelude.. Lens.mapping Prelude._Coerce

-- | The type of attribute or attributes that were removed from the
-- endpoints. Valid values are:
--
-- -   endpoint-custom-attributes - Custom attributes that describe
--     endpoints.
--
-- -   endpoint-metric-attributes - Custom metrics that your app reports to
--     Amazon Pinpoint for endpoints.
--
-- -   endpoint-user-attributes - Custom attributes that describe users.
attributesResource_attributeType :: Lens.Lens' AttributesResource Prelude.Text
attributesResource_attributeType = Lens.lens (\AttributesResource' {attributeType} -> attributeType) (\s@AttributesResource' {} a -> s {attributeType = a} :: AttributesResource)

-- | The unique identifier for the application.
attributesResource_applicationId :: Lens.Lens' AttributesResource Prelude.Text
attributesResource_applicationId = Lens.lens (\AttributesResource' {applicationId} -> applicationId) (\s@AttributesResource' {} a -> s {applicationId = a} :: AttributesResource)

instance Prelude.FromJSON AttributesResource where
  parseJSON =
    Prelude.withObject
      "AttributesResource"
      ( \x ->
          AttributesResource'
            Prelude.<$> ( x Prelude..:? "Attributes"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "AttributeType")
            Prelude.<*> (x Prelude..: "ApplicationId")
      )

instance Prelude.Hashable AttributesResource

instance Prelude.NFData AttributesResource

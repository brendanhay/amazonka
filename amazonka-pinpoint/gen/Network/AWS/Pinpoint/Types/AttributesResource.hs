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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about the type and the names of attributes that
-- were removed from all the endpoints that are associated with an
-- application.
--
-- /See:/ 'newAttributesResource' smart constructor.
data AttributesResource = AttributesResource'
  { -- | An array that specifies the names of the attributes that were removed
    -- from the endpoints.
    attributes :: Core.Maybe [Core.Text],
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
    attributeType :: Core.Text,
    -- | The unique identifier for the application.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  AttributesResource
newAttributesResource pAttributeType_ pApplicationId_ =
  AttributesResource'
    { attributes = Core.Nothing,
      attributeType = pAttributeType_,
      applicationId = pApplicationId_
    }

-- | An array that specifies the names of the attributes that were removed
-- from the endpoints.
attributesResource_attributes :: Lens.Lens' AttributesResource (Core.Maybe [Core.Text])
attributesResource_attributes = Lens.lens (\AttributesResource' {attributes} -> attributes) (\s@AttributesResource' {} a -> s {attributes = a} :: AttributesResource) Core.. Lens.mapping Lens._Coerce

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
attributesResource_attributeType :: Lens.Lens' AttributesResource Core.Text
attributesResource_attributeType = Lens.lens (\AttributesResource' {attributeType} -> attributeType) (\s@AttributesResource' {} a -> s {attributeType = a} :: AttributesResource)

-- | The unique identifier for the application.
attributesResource_applicationId :: Lens.Lens' AttributesResource Core.Text
attributesResource_applicationId = Lens.lens (\AttributesResource' {applicationId} -> applicationId) (\s@AttributesResource' {} a -> s {applicationId = a} :: AttributesResource)

instance Core.FromJSON AttributesResource where
  parseJSON =
    Core.withObject
      "AttributesResource"
      ( \x ->
          AttributesResource'
            Core.<$> (x Core..:? "Attributes" Core..!= Core.mempty)
            Core.<*> (x Core..: "AttributeType")
            Core.<*> (x Core..: "ApplicationId")
      )

instance Core.Hashable AttributesResource

instance Core.NFData AttributesResource

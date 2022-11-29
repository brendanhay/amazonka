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
-- Module      : Amazonka.ResourceExplorer2.Types.SupportedResourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceExplorer2.Types.SupportedResourceType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure that describes a resource type supported by Amazon Web
-- Services Resource Explorer.
--
-- /See:/ 'newSupportedResourceType' smart constructor.
data SupportedResourceType = SupportedResourceType'
  { -- | The unique identifier of the resource type.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Service that is associated with the resource type. This
    -- is the primary service that lets you create and interact with resources
    -- of this type.
    service :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SupportedResourceType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'supportedResourceType_resourceType' - The unique identifier of the resource type.
--
-- 'service', 'supportedResourceType_service' - The Amazon Web Service that is associated with the resource type. This
-- is the primary service that lets you create and interact with resources
-- of this type.
newSupportedResourceType ::
  SupportedResourceType
newSupportedResourceType =
  SupportedResourceType'
    { resourceType =
        Prelude.Nothing,
      service = Prelude.Nothing
    }

-- | The unique identifier of the resource type.
supportedResourceType_resourceType :: Lens.Lens' SupportedResourceType (Prelude.Maybe Prelude.Text)
supportedResourceType_resourceType = Lens.lens (\SupportedResourceType' {resourceType} -> resourceType) (\s@SupportedResourceType' {} a -> s {resourceType = a} :: SupportedResourceType)

-- | The Amazon Web Service that is associated with the resource type. This
-- is the primary service that lets you create and interact with resources
-- of this type.
supportedResourceType_service :: Lens.Lens' SupportedResourceType (Prelude.Maybe Prelude.Text)
supportedResourceType_service = Lens.lens (\SupportedResourceType' {service} -> service) (\s@SupportedResourceType' {} a -> s {service = a} :: SupportedResourceType)

instance Core.FromJSON SupportedResourceType where
  parseJSON =
    Core.withObject
      "SupportedResourceType"
      ( \x ->
          SupportedResourceType'
            Prelude.<$> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "Service")
      )

instance Prelude.Hashable SupportedResourceType where
  hashWithSalt _salt SupportedResourceType' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` service

instance Prelude.NFData SupportedResourceType where
  rnf SupportedResourceType' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf service

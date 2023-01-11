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
-- Module      : Amazonka.Config.Types.RemediationExceptionResourceKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.RemediationExceptionResourceKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details that identify a resource within Config, including the
-- resource type and resource ID.
--
-- /See:/ 'newRemediationExceptionResourceKey' smart constructor.
data RemediationExceptionResourceKey = RemediationExceptionResourceKey'
  { -- | The ID of the resource (for example., sg-xxxxxx).
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of a resource.
    resourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemediationExceptionResourceKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'remediationExceptionResourceKey_resourceId' - The ID of the resource (for example., sg-xxxxxx).
--
-- 'resourceType', 'remediationExceptionResourceKey_resourceType' - The type of a resource.
newRemediationExceptionResourceKey ::
  RemediationExceptionResourceKey
newRemediationExceptionResourceKey =
  RemediationExceptionResourceKey'
    { resourceId =
        Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The ID of the resource (for example., sg-xxxxxx).
remediationExceptionResourceKey_resourceId :: Lens.Lens' RemediationExceptionResourceKey (Prelude.Maybe Prelude.Text)
remediationExceptionResourceKey_resourceId = Lens.lens (\RemediationExceptionResourceKey' {resourceId} -> resourceId) (\s@RemediationExceptionResourceKey' {} a -> s {resourceId = a} :: RemediationExceptionResourceKey)

-- | The type of a resource.
remediationExceptionResourceKey_resourceType :: Lens.Lens' RemediationExceptionResourceKey (Prelude.Maybe Prelude.Text)
remediationExceptionResourceKey_resourceType = Lens.lens (\RemediationExceptionResourceKey' {resourceType} -> resourceType) (\s@RemediationExceptionResourceKey' {} a -> s {resourceType = a} :: RemediationExceptionResourceKey)

instance
  Data.FromJSON
    RemediationExceptionResourceKey
  where
  parseJSON =
    Data.withObject
      "RemediationExceptionResourceKey"
      ( \x ->
          RemediationExceptionResourceKey'
            Prelude.<$> (x Data..:? "ResourceId")
            Prelude.<*> (x Data..:? "ResourceType")
      )

instance
  Prelude.Hashable
    RemediationExceptionResourceKey
  where
  hashWithSalt
    _salt
    RemediationExceptionResourceKey' {..} =
      _salt `Prelude.hashWithSalt` resourceId
        `Prelude.hashWithSalt` resourceType

instance
  Prelude.NFData
    RemediationExceptionResourceKey
  where
  rnf RemediationExceptionResourceKey' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToJSON RemediationExceptionResourceKey where
  toJSON RemediationExceptionResourceKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ResourceId" Data..=) Prelude.<$> resourceId,
            ("ResourceType" Data..=) Prelude.<$> resourceType
          ]
      )

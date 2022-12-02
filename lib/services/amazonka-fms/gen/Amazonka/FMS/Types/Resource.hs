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
-- Module      : Amazonka.FMS.Types.Resource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details of a resource that is associated to an Firewall Manager resource
-- set.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | The Amazon Web Services account ID that the associated resource belongs
    -- to.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The resource\'s universal resource indicator (URI).
    uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'resource_accountId' - The Amazon Web Services account ID that the associated resource belongs
-- to.
--
-- 'uri', 'resource_uri' - The resource\'s universal resource indicator (URI).
newResource ::
  -- | 'uri'
  Prelude.Text ->
  Resource
newResource pURI_ =
  Resource' {accountId = Prelude.Nothing, uri = pURI_}

-- | The Amazon Web Services account ID that the associated resource belongs
-- to.
resource_accountId :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_accountId = Lens.lens (\Resource' {accountId} -> accountId) (\s@Resource' {} a -> s {accountId = a} :: Resource)

-- | The resource\'s universal resource indicator (URI).
resource_uri :: Lens.Lens' Resource Prelude.Text
resource_uri = Lens.lens (\Resource' {uri} -> uri) (\s@Resource' {} a -> s {uri = a} :: Resource)

instance Data.FromJSON Resource where
  parseJSON =
    Data.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..: "URI")
      )

instance Prelude.Hashable Resource where
  hashWithSalt _salt Resource' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` uri

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf accountId `Prelude.seq` Prelude.rnf uri

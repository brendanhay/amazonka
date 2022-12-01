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
-- Module      : Amazonka.AppFlow.Types.SingularConnectorProfileProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SingularConnectorProfileProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile properties required when using Singular.
--
-- /See:/ 'newSingularConnectorProfileProperties' smart constructor.
data SingularConnectorProfileProperties = SingularConnectorProfileProperties'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SingularConnectorProfileProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSingularConnectorProfileProperties ::
  SingularConnectorProfileProperties
newSingularConnectorProfileProperties =
  SingularConnectorProfileProperties'

instance
  Core.FromJSON
    SingularConnectorProfileProperties
  where
  parseJSON =
    Core.withObject
      "SingularConnectorProfileProperties"
      ( \x ->
          Prelude.pure SingularConnectorProfileProperties'
      )

instance
  Prelude.Hashable
    SingularConnectorProfileProperties
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    SingularConnectorProfileProperties
  where
  rnf _ = ()

instance
  Core.ToJSON
    SingularConnectorProfileProperties
  where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

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
-- Module      : Network.AWS.Route53AutoNaming.Types.HttpProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.HttpProperties where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex type that contains the name of an HTTP namespace.
--
-- /See:/ 'newHttpProperties' smart constructor.
data HttpProperties = HttpProperties'
  { -- | The name of an HTTP namespace.
    httpName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpName', 'httpProperties_httpName' - The name of an HTTP namespace.
newHttpProperties ::
  HttpProperties
newHttpProperties =
  HttpProperties' {httpName = Prelude.Nothing}

-- | The name of an HTTP namespace.
httpProperties_httpName :: Lens.Lens' HttpProperties (Prelude.Maybe Prelude.Text)
httpProperties_httpName = Lens.lens (\HttpProperties' {httpName} -> httpName) (\s@HttpProperties' {} a -> s {httpName = a} :: HttpProperties)

instance Core.FromJSON HttpProperties where
  parseJSON =
    Core.withObject
      "HttpProperties"
      ( \x ->
          HttpProperties' Prelude.<$> (x Core..:? "HttpName")
      )

instance Prelude.Hashable HttpProperties

instance Prelude.NFData HttpProperties

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
-- Module      : Amazonka.AppRunner.Types.IngressConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.IngressConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Network configuration settings for inbound network traffic.
--
-- /See:/ 'newIngressConfiguration' smart constructor.
data IngressConfiguration = IngressConfiguration'
  { -- | Specifies whether your App Runner service is publicly accessible. To
    -- make the service publicly accessible set it to @True@. To make the
    -- service privately accessible, from only within an Amazon VPC set it to
    -- @False@.
    isPubliclyAccessible :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IngressConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isPubliclyAccessible', 'ingressConfiguration_isPubliclyAccessible' - Specifies whether your App Runner service is publicly accessible. To
-- make the service publicly accessible set it to @True@. To make the
-- service privately accessible, from only within an Amazon VPC set it to
-- @False@.
newIngressConfiguration ::
  IngressConfiguration
newIngressConfiguration =
  IngressConfiguration'
    { isPubliclyAccessible =
        Prelude.Nothing
    }

-- | Specifies whether your App Runner service is publicly accessible. To
-- make the service publicly accessible set it to @True@. To make the
-- service privately accessible, from only within an Amazon VPC set it to
-- @False@.
ingressConfiguration_isPubliclyAccessible :: Lens.Lens' IngressConfiguration (Prelude.Maybe Prelude.Bool)
ingressConfiguration_isPubliclyAccessible = Lens.lens (\IngressConfiguration' {isPubliclyAccessible} -> isPubliclyAccessible) (\s@IngressConfiguration' {} a -> s {isPubliclyAccessible = a} :: IngressConfiguration)

instance Data.FromJSON IngressConfiguration where
  parseJSON =
    Data.withObject
      "IngressConfiguration"
      ( \x ->
          IngressConfiguration'
            Prelude.<$> (x Data..:? "IsPubliclyAccessible")
      )

instance Prelude.Hashable IngressConfiguration where
  hashWithSalt _salt IngressConfiguration' {..} =
    _salt `Prelude.hashWithSalt` isPubliclyAccessible

instance Prelude.NFData IngressConfiguration where
  rnf IngressConfiguration' {..} =
    Prelude.rnf isPubliclyAccessible

instance Data.ToJSON IngressConfiguration where
  toJSON IngressConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IsPubliclyAccessible" Data..=)
              Prelude.<$> isPubliclyAccessible
          ]
      )

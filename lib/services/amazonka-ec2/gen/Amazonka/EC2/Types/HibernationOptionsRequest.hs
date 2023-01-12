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
-- Module      : Amazonka.EC2.Types.HibernationOptionsRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.HibernationOptionsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether your instance is configured for hibernation. This
-- parameter is valid only if the instance meets the
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance>
-- in the /Amazon EC2 User Guide/.
--
-- /See:/ 'newHibernationOptionsRequest' smart constructor.
data HibernationOptionsRequest = HibernationOptionsRequest'
  { -- | If you set this parameter to @true@, your instance is enabled for
    -- hibernation.
    --
    -- Default: @false@
    configured :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HibernationOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configured', 'hibernationOptionsRequest_configured' - If you set this parameter to @true@, your instance is enabled for
-- hibernation.
--
-- Default: @false@
newHibernationOptionsRequest ::
  HibernationOptionsRequest
newHibernationOptionsRequest =
  HibernationOptionsRequest'
    { configured =
        Prelude.Nothing
    }

-- | If you set this parameter to @true@, your instance is enabled for
-- hibernation.
--
-- Default: @false@
hibernationOptionsRequest_configured :: Lens.Lens' HibernationOptionsRequest (Prelude.Maybe Prelude.Bool)
hibernationOptionsRequest_configured = Lens.lens (\HibernationOptionsRequest' {configured} -> configured) (\s@HibernationOptionsRequest' {} a -> s {configured = a} :: HibernationOptionsRequest)

instance Prelude.Hashable HibernationOptionsRequest where
  hashWithSalt _salt HibernationOptionsRequest' {..} =
    _salt `Prelude.hashWithSalt` configured

instance Prelude.NFData HibernationOptionsRequest where
  rnf HibernationOptionsRequest' {..} =
    Prelude.rnf configured

instance Data.ToQuery HibernationOptionsRequest where
  toQuery HibernationOptionsRequest' {..} =
    Prelude.mconcat ["Configured" Data.=: configured]

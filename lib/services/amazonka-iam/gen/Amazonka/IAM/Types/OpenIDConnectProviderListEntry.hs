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
-- Module      : Amazonka.IAM.Types.OpenIDConnectProviderListEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.OpenIDConnectProviderListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the Amazon Resource Name (ARN) for an IAM OpenID Connect
-- provider.
--
-- /See:/ 'newOpenIDConnectProviderListEntry' smart constructor.
data OpenIDConnectProviderListEntry = OpenIDConnectProviderListEntry'
  { arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpenIDConnectProviderListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'openIDConnectProviderListEntry_arn' - Undocumented member.
newOpenIDConnectProviderListEntry ::
  OpenIDConnectProviderListEntry
newOpenIDConnectProviderListEntry =
  OpenIDConnectProviderListEntry'
    { arn =
        Prelude.Nothing
    }

-- | Undocumented member.
openIDConnectProviderListEntry_arn :: Lens.Lens' OpenIDConnectProviderListEntry (Prelude.Maybe Prelude.Text)
openIDConnectProviderListEntry_arn = Lens.lens (\OpenIDConnectProviderListEntry' {arn} -> arn) (\s@OpenIDConnectProviderListEntry' {} a -> s {arn = a} :: OpenIDConnectProviderListEntry)

instance Data.FromXML OpenIDConnectProviderListEntry where
  parseXML x =
    OpenIDConnectProviderListEntry'
      Prelude.<$> (x Data..@? "Arn")

instance
  Prelude.Hashable
    OpenIDConnectProviderListEntry
  where
  hashWithSalt
    _salt
    OpenIDConnectProviderListEntry' {..} =
      _salt `Prelude.hashWithSalt` arn

instance
  Prelude.NFData
    OpenIDConnectProviderListEntry
  where
  rnf OpenIDConnectProviderListEntry' {..} =
    Prelude.rnf arn

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
-- Module      : Network.AWS.IAM.Types.OpenIDConnectProviderListEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.OpenIDConnectProviderListEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the Amazon Resource Name (ARN) for an IAM OpenID Connect
-- provider.
--
-- /See:/ 'newOpenIDConnectProviderListEntry' smart constructor.
data OpenIDConnectProviderListEntry = OpenIDConnectProviderListEntry'
  { arn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  OpenIDConnectProviderListEntry' {arn = Core.Nothing}

-- | Undocumented member.
openIDConnectProviderListEntry_arn :: Lens.Lens' OpenIDConnectProviderListEntry (Core.Maybe Core.Text)
openIDConnectProviderListEntry_arn = Lens.lens (\OpenIDConnectProviderListEntry' {arn} -> arn) (\s@OpenIDConnectProviderListEntry' {} a -> s {arn = a} :: OpenIDConnectProviderListEntry)

instance Core.FromXML OpenIDConnectProviderListEntry where
  parseXML x =
    OpenIDConnectProviderListEntry'
      Core.<$> (x Core..@? "Arn")

instance Core.Hashable OpenIDConnectProviderListEntry

instance Core.NFData OpenIDConnectProviderListEntry

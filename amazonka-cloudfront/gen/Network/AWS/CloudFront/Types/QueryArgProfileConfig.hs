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
-- Module      : Network.AWS.CloudFront.Types.QueryArgProfileConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.QueryArgProfileConfig where

import Network.AWS.CloudFront.Types.QueryArgProfiles
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configuration for query argument-profile mapping for field-level
-- encryption.
--
-- /See:/ 'newQueryArgProfileConfig' smart constructor.
data QueryArgProfileConfig = QueryArgProfileConfig'
  { -- | Profiles specified for query argument-profile mapping for field-level
    -- encryption.
    queryArgProfiles :: Core.Maybe QueryArgProfiles,
    -- | Flag to set if you want a request to be forwarded to the origin even if
    -- the profile specified by the field-level encryption query argument,
    -- fle-profile, is unknown.
    forwardWhenQueryArgProfileIsUnknown :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'QueryArgProfileConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryArgProfiles', 'queryArgProfileConfig_queryArgProfiles' - Profiles specified for query argument-profile mapping for field-level
-- encryption.
--
-- 'forwardWhenQueryArgProfileIsUnknown', 'queryArgProfileConfig_forwardWhenQueryArgProfileIsUnknown' - Flag to set if you want a request to be forwarded to the origin even if
-- the profile specified by the field-level encryption query argument,
-- fle-profile, is unknown.
newQueryArgProfileConfig ::
  -- | 'forwardWhenQueryArgProfileIsUnknown'
  Core.Bool ->
  QueryArgProfileConfig
newQueryArgProfileConfig
  pForwardWhenQueryArgProfileIsUnknown_ =
    QueryArgProfileConfig'
      { queryArgProfiles =
          Core.Nothing,
        forwardWhenQueryArgProfileIsUnknown =
          pForwardWhenQueryArgProfileIsUnknown_
      }

-- | Profiles specified for query argument-profile mapping for field-level
-- encryption.
queryArgProfileConfig_queryArgProfiles :: Lens.Lens' QueryArgProfileConfig (Core.Maybe QueryArgProfiles)
queryArgProfileConfig_queryArgProfiles = Lens.lens (\QueryArgProfileConfig' {queryArgProfiles} -> queryArgProfiles) (\s@QueryArgProfileConfig' {} a -> s {queryArgProfiles = a} :: QueryArgProfileConfig)

-- | Flag to set if you want a request to be forwarded to the origin even if
-- the profile specified by the field-level encryption query argument,
-- fle-profile, is unknown.
queryArgProfileConfig_forwardWhenQueryArgProfileIsUnknown :: Lens.Lens' QueryArgProfileConfig Core.Bool
queryArgProfileConfig_forwardWhenQueryArgProfileIsUnknown = Lens.lens (\QueryArgProfileConfig' {forwardWhenQueryArgProfileIsUnknown} -> forwardWhenQueryArgProfileIsUnknown) (\s@QueryArgProfileConfig' {} a -> s {forwardWhenQueryArgProfileIsUnknown = a} :: QueryArgProfileConfig)

instance Core.FromXML QueryArgProfileConfig where
  parseXML x =
    QueryArgProfileConfig'
      Core.<$> (x Core..@? "QueryArgProfiles")
      Core.<*> (x Core..@ "ForwardWhenQueryArgProfileIsUnknown")

instance Core.Hashable QueryArgProfileConfig

instance Core.NFData QueryArgProfileConfig

instance Core.ToXML QueryArgProfileConfig where
  toXML QueryArgProfileConfig' {..} =
    Core.mconcat
      [ "QueryArgProfiles" Core.@= queryArgProfiles,
        "ForwardWhenQueryArgProfileIsUnknown"
          Core.@= forwardWhenQueryArgProfileIsUnknown
      ]

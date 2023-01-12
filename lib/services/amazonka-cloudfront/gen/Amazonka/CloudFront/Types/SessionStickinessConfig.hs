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
-- Module      : Amazonka.CloudFront.Types.SessionStickinessConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.SessionStickinessConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Session stickiness provides the ability to define multiple requests from
-- a single viewer as a single session. This prevents the potentially
-- inconsistent experience of sending some of a given user\'s requests to
-- your staging distribution, while others are sent to your primary
-- distribution. Define the session duration using TTL values.
--
-- /See:/ 'newSessionStickinessConfig' smart constructor.
data SessionStickinessConfig = SessionStickinessConfig'
  { -- | The amount of time after which you want sessions to cease if no requests
    -- are received. Allowed values are 300–3600 seconds (5–60 minutes).
    --
    -- The value must be less than or equal to @MaximumTTL@.
    idleTTL :: Prelude.Int,
    -- | The maximum amount of time to consider requests from the viewer as being
    -- part of the same session. Allowed values are 300–3600 seconds (5–60
    -- minutes).
    --
    -- The value must be less than or equal to @IdleTTL@.
    maximumTTL :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionStickinessConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idleTTL', 'sessionStickinessConfig_idleTTL' - The amount of time after which you want sessions to cease if no requests
-- are received. Allowed values are 300–3600 seconds (5–60 minutes).
--
-- The value must be less than or equal to @MaximumTTL@.
--
-- 'maximumTTL', 'sessionStickinessConfig_maximumTTL' - The maximum amount of time to consider requests from the viewer as being
-- part of the same session. Allowed values are 300–3600 seconds (5–60
-- minutes).
--
-- The value must be less than or equal to @IdleTTL@.
newSessionStickinessConfig ::
  -- | 'idleTTL'
  Prelude.Int ->
  -- | 'maximumTTL'
  Prelude.Int ->
  SessionStickinessConfig
newSessionStickinessConfig pIdleTTL_ pMaximumTTL_ =
  SessionStickinessConfig'
    { idleTTL = pIdleTTL_,
      maximumTTL = pMaximumTTL_
    }

-- | The amount of time after which you want sessions to cease if no requests
-- are received. Allowed values are 300–3600 seconds (5–60 minutes).
--
-- The value must be less than or equal to @MaximumTTL@.
sessionStickinessConfig_idleTTL :: Lens.Lens' SessionStickinessConfig Prelude.Int
sessionStickinessConfig_idleTTL = Lens.lens (\SessionStickinessConfig' {idleTTL} -> idleTTL) (\s@SessionStickinessConfig' {} a -> s {idleTTL = a} :: SessionStickinessConfig)

-- | The maximum amount of time to consider requests from the viewer as being
-- part of the same session. Allowed values are 300–3600 seconds (5–60
-- minutes).
--
-- The value must be less than or equal to @IdleTTL@.
sessionStickinessConfig_maximumTTL :: Lens.Lens' SessionStickinessConfig Prelude.Int
sessionStickinessConfig_maximumTTL = Lens.lens (\SessionStickinessConfig' {maximumTTL} -> maximumTTL) (\s@SessionStickinessConfig' {} a -> s {maximumTTL = a} :: SessionStickinessConfig)

instance Data.FromXML SessionStickinessConfig where
  parseXML x =
    SessionStickinessConfig'
      Prelude.<$> (x Data..@ "IdleTTL")
      Prelude.<*> (x Data..@ "MaximumTTL")

instance Prelude.Hashable SessionStickinessConfig where
  hashWithSalt _salt SessionStickinessConfig' {..} =
    _salt `Prelude.hashWithSalt` idleTTL
      `Prelude.hashWithSalt` maximumTTL

instance Prelude.NFData SessionStickinessConfig where
  rnf SessionStickinessConfig' {..} =
    Prelude.rnf idleTTL
      `Prelude.seq` Prelude.rnf maximumTTL

instance Data.ToXML SessionStickinessConfig where
  toXML SessionStickinessConfig' {..} =
    Prelude.mconcat
      [ "IdleTTL" Data.@= idleTTL,
        "MaximumTTL" Data.@= maximumTTL
      ]

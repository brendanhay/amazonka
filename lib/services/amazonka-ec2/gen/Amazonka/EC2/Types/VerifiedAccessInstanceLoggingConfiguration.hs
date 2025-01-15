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
-- Module      : Amazonka.EC2.Types.VerifiedAccessInstanceLoggingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessInstanceLoggingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VerifiedAccessLogs
import qualified Amazonka.Prelude as Prelude

-- | Describes logging options for an Amazon Web Services Verified Access
-- instance.
--
-- /See:/ 'newVerifiedAccessInstanceLoggingConfiguration' smart constructor.
data VerifiedAccessInstanceLoggingConfiguration = VerifiedAccessInstanceLoggingConfiguration'
  { -- | Details about the logging options.
    accessLogs :: Prelude.Maybe VerifiedAccessLogs,
    -- | The ID of the Amazon Web Services Verified Access instance.
    verifiedAccessInstanceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifiedAccessInstanceLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessLogs', 'verifiedAccessInstanceLoggingConfiguration_accessLogs' - Details about the logging options.
--
-- 'verifiedAccessInstanceId', 'verifiedAccessInstanceLoggingConfiguration_verifiedAccessInstanceId' - The ID of the Amazon Web Services Verified Access instance.
newVerifiedAccessInstanceLoggingConfiguration ::
  VerifiedAccessInstanceLoggingConfiguration
newVerifiedAccessInstanceLoggingConfiguration =
  VerifiedAccessInstanceLoggingConfiguration'
    { accessLogs =
        Prelude.Nothing,
      verifiedAccessInstanceId =
        Prelude.Nothing
    }

-- | Details about the logging options.
verifiedAccessInstanceLoggingConfiguration_accessLogs :: Lens.Lens' VerifiedAccessInstanceLoggingConfiguration (Prelude.Maybe VerifiedAccessLogs)
verifiedAccessInstanceLoggingConfiguration_accessLogs = Lens.lens (\VerifiedAccessInstanceLoggingConfiguration' {accessLogs} -> accessLogs) (\s@VerifiedAccessInstanceLoggingConfiguration' {} a -> s {accessLogs = a} :: VerifiedAccessInstanceLoggingConfiguration)

-- | The ID of the Amazon Web Services Verified Access instance.
verifiedAccessInstanceLoggingConfiguration_verifiedAccessInstanceId :: Lens.Lens' VerifiedAccessInstanceLoggingConfiguration (Prelude.Maybe Prelude.Text)
verifiedAccessInstanceLoggingConfiguration_verifiedAccessInstanceId = Lens.lens (\VerifiedAccessInstanceLoggingConfiguration' {verifiedAccessInstanceId} -> verifiedAccessInstanceId) (\s@VerifiedAccessInstanceLoggingConfiguration' {} a -> s {verifiedAccessInstanceId = a} :: VerifiedAccessInstanceLoggingConfiguration)

instance
  Data.FromXML
    VerifiedAccessInstanceLoggingConfiguration
  where
  parseXML x =
    VerifiedAccessInstanceLoggingConfiguration'
      Prelude.<$> (x Data..@? "accessLogs")
      Prelude.<*> (x Data..@? "verifiedAccessInstanceId")

instance
  Prelude.Hashable
    VerifiedAccessInstanceLoggingConfiguration
  where
  hashWithSalt
    _salt
    VerifiedAccessInstanceLoggingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` accessLogs
        `Prelude.hashWithSalt` verifiedAccessInstanceId

instance
  Prelude.NFData
    VerifiedAccessInstanceLoggingConfiguration
  where
  rnf VerifiedAccessInstanceLoggingConfiguration' {..} =
    Prelude.rnf accessLogs `Prelude.seq`
      Prelude.rnf verifiedAccessInstanceId

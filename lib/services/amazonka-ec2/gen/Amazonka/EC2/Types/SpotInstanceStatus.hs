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
-- Module      : Amazonka.EC2.Types.SpotInstanceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotInstanceStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the status of a Spot Instance request.
--
-- /See:/ 'newSpotInstanceStatus' smart constructor.
data SpotInstanceStatus = SpotInstanceStatus'
  { -- | The status code. For a list of status codes, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-request-status.html#spot-instance-request-status-understand Spot request status codes>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    code :: Prelude.Maybe Prelude.Text,
    -- | The description for the status code.
    message :: Prelude.Maybe Prelude.Text,
    -- | The date and time of the most recent status update, in UTC format (for
    -- example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    updateTime :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpotInstanceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'spotInstanceStatus_code' - The status code. For a list of status codes, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-request-status.html#spot-instance-request-status-understand Spot request status codes>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'message', 'spotInstanceStatus_message' - The description for the status code.
--
-- 'updateTime', 'spotInstanceStatus_updateTime' - The date and time of the most recent status update, in UTC format (for
-- example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
newSpotInstanceStatus ::
  SpotInstanceStatus
newSpotInstanceStatus =
  SpotInstanceStatus'
    { code = Prelude.Nothing,
      message = Prelude.Nothing,
      updateTime = Prelude.Nothing
    }

-- | The status code. For a list of status codes, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-request-status.html#spot-instance-request-status-understand Spot request status codes>
-- in the /Amazon EC2 User Guide for Linux Instances/.
spotInstanceStatus_code :: Lens.Lens' SpotInstanceStatus (Prelude.Maybe Prelude.Text)
spotInstanceStatus_code = Lens.lens (\SpotInstanceStatus' {code} -> code) (\s@SpotInstanceStatus' {} a -> s {code = a} :: SpotInstanceStatus)

-- | The description for the status code.
spotInstanceStatus_message :: Lens.Lens' SpotInstanceStatus (Prelude.Maybe Prelude.Text)
spotInstanceStatus_message = Lens.lens (\SpotInstanceStatus' {message} -> message) (\s@SpotInstanceStatus' {} a -> s {message = a} :: SpotInstanceStatus)

-- | The date and time of the most recent status update, in UTC format (for
-- example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
spotInstanceStatus_updateTime :: Lens.Lens' SpotInstanceStatus (Prelude.Maybe Prelude.UTCTime)
spotInstanceStatus_updateTime = Lens.lens (\SpotInstanceStatus' {updateTime} -> updateTime) (\s@SpotInstanceStatus' {} a -> s {updateTime = a} :: SpotInstanceStatus) Prelude.. Lens.mapping Data._Time

instance Data.FromXML SpotInstanceStatus where
  parseXML x =
    SpotInstanceStatus'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")
      Prelude.<*> (x Data..@? "updateTime")

instance Prelude.Hashable SpotInstanceStatus where
  hashWithSalt _salt SpotInstanceStatus' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData SpotInstanceStatus where
  rnf SpotInstanceStatus' {..} =
    Prelude.rnf code
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf updateTime

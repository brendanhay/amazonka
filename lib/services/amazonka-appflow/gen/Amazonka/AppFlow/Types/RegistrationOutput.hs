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
-- Module      : Amazonka.AppFlow.Types.RegistrationOutput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.RegistrationOutput where

import Amazonka.AppFlow.Types.ExecutionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the status of an attempt from Amazon AppFlow to register a
-- resource.
--
-- When you run a flow that you\'ve configured to use a metadata catalog,
-- Amazon AppFlow registers a metadata table and data partitions with that
-- catalog. This operation provides the status of that registration
-- attempt. The operation also indicates how many related resources Amazon
-- AppFlow created or updated.
--
-- /See:/ 'newRegistrationOutput' smart constructor.
data RegistrationOutput = RegistrationOutput'
  { -- | Explains the status of the registration attempt from Amazon AppFlow. If
    -- the attempt fails, the message explains why.
    message :: Prelude.Maybe Prelude.Text,
    -- | Indicates the status of the registration attempt from Amazon AppFlow.
    status :: Prelude.Maybe ExecutionStatus,
    -- | Indicates the number of resources that Amazon AppFlow created or
    -- updated. Possible resources include metadata tables and data partitions.
    result :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegistrationOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'registrationOutput_message' - Explains the status of the registration attempt from Amazon AppFlow. If
-- the attempt fails, the message explains why.
--
-- 'status', 'registrationOutput_status' - Indicates the status of the registration attempt from Amazon AppFlow.
--
-- 'result', 'registrationOutput_result' - Indicates the number of resources that Amazon AppFlow created or
-- updated. Possible resources include metadata tables and data partitions.
newRegistrationOutput ::
  RegistrationOutput
newRegistrationOutput =
  RegistrationOutput'
    { message = Prelude.Nothing,
      status = Prelude.Nothing,
      result = Prelude.Nothing
    }

-- | Explains the status of the registration attempt from Amazon AppFlow. If
-- the attempt fails, the message explains why.
registrationOutput_message :: Lens.Lens' RegistrationOutput (Prelude.Maybe Prelude.Text)
registrationOutput_message = Lens.lens (\RegistrationOutput' {message} -> message) (\s@RegistrationOutput' {} a -> s {message = a} :: RegistrationOutput)

-- | Indicates the status of the registration attempt from Amazon AppFlow.
registrationOutput_status :: Lens.Lens' RegistrationOutput (Prelude.Maybe ExecutionStatus)
registrationOutput_status = Lens.lens (\RegistrationOutput' {status} -> status) (\s@RegistrationOutput' {} a -> s {status = a} :: RegistrationOutput)

-- | Indicates the number of resources that Amazon AppFlow created or
-- updated. Possible resources include metadata tables and data partitions.
registrationOutput_result :: Lens.Lens' RegistrationOutput (Prelude.Maybe Prelude.Text)
registrationOutput_result = Lens.lens (\RegistrationOutput' {result} -> result) (\s@RegistrationOutput' {} a -> s {result = a} :: RegistrationOutput)

instance Data.FromJSON RegistrationOutput where
  parseJSON =
    Data.withObject
      "RegistrationOutput"
      ( \x ->
          RegistrationOutput'
            Prelude.<$> (x Data..:? "message")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "result")
      )

instance Prelude.Hashable RegistrationOutput where
  hashWithSalt _salt RegistrationOutput' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` result

instance Prelude.NFData RegistrationOutput where
  rnf RegistrationOutput' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf result

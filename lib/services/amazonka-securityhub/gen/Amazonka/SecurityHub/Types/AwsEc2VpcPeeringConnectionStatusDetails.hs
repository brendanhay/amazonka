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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2VpcPeeringConnectionStatusDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2VpcPeeringConnectionStatusDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the status of the VPC peering connection.
--
-- /See:/ 'newAwsEc2VpcPeeringConnectionStatusDetails' smart constructor.
data AwsEc2VpcPeeringConnectionStatusDetails = AwsEc2VpcPeeringConnectionStatusDetails'
  { -- | A message that provides more information about the status, if
    -- applicable.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status of the VPC peering connection.
    code :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2VpcPeeringConnectionStatusDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'awsEc2VpcPeeringConnectionStatusDetails_message' - A message that provides more information about the status, if
-- applicable.
--
-- 'code', 'awsEc2VpcPeeringConnectionStatusDetails_code' - The status of the VPC peering connection.
newAwsEc2VpcPeeringConnectionStatusDetails ::
  AwsEc2VpcPeeringConnectionStatusDetails
newAwsEc2VpcPeeringConnectionStatusDetails =
  AwsEc2VpcPeeringConnectionStatusDetails'
    { message =
        Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | A message that provides more information about the status, if
-- applicable.
awsEc2VpcPeeringConnectionStatusDetails_message :: Lens.Lens' AwsEc2VpcPeeringConnectionStatusDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcPeeringConnectionStatusDetails_message = Lens.lens (\AwsEc2VpcPeeringConnectionStatusDetails' {message} -> message) (\s@AwsEc2VpcPeeringConnectionStatusDetails' {} a -> s {message = a} :: AwsEc2VpcPeeringConnectionStatusDetails)

-- | The status of the VPC peering connection.
awsEc2VpcPeeringConnectionStatusDetails_code :: Lens.Lens' AwsEc2VpcPeeringConnectionStatusDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcPeeringConnectionStatusDetails_code = Lens.lens (\AwsEc2VpcPeeringConnectionStatusDetails' {code} -> code) (\s@AwsEc2VpcPeeringConnectionStatusDetails' {} a -> s {code = a} :: AwsEc2VpcPeeringConnectionStatusDetails)

instance
  Data.FromJSON
    AwsEc2VpcPeeringConnectionStatusDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2VpcPeeringConnectionStatusDetails"
      ( \x ->
          AwsEc2VpcPeeringConnectionStatusDetails'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Code")
      )

instance
  Prelude.Hashable
    AwsEc2VpcPeeringConnectionStatusDetails
  where
  hashWithSalt
    _salt
    AwsEc2VpcPeeringConnectionStatusDetails' {..} =
      _salt `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` code

instance
  Prelude.NFData
    AwsEc2VpcPeeringConnectionStatusDetails
  where
  rnf AwsEc2VpcPeeringConnectionStatusDetails' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf code

instance
  Data.ToJSON
    AwsEc2VpcPeeringConnectionStatusDetails
  where
  toJSON AwsEc2VpcPeeringConnectionStatusDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Message" Data..=) Prelude.<$> message,
            ("Code" Data..=) Prelude.<$> code
          ]
      )

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
-- Module      : Amazonka.Inspector2.Types.Ec2InstanceAggregationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Ec2InstanceAggregationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.SeverityCounts
import qualified Amazonka.Prelude as Prelude

-- | A response that contains the results of a finding aggregation by Amazon
-- EC2 instance.
--
-- /See:/ 'newEc2InstanceAggregationResponse' smart constructor.
data Ec2InstanceAggregationResponse = Ec2InstanceAggregationResponse'
  { -- | The Amazon Web Services account for the Amazon EC2 instance.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Machine Image (AMI) of the Amazon EC2 instance.
    ami :: Prelude.Maybe Prelude.Text,
    -- | The tags attached to the instance.
    instanceTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The number of network findings for the Amazon EC2 instance.
    networkFindings :: Prelude.Maybe Prelude.Integer,
    -- | The operating system of the Amazon EC2 instance.
    operatingSystem :: Prelude.Maybe Prelude.Text,
    -- | An object that contains the count of matched findings per severity.
    severityCounts :: Prelude.Maybe SeverityCounts,
    -- | The Amazon EC2 instance ID.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ec2InstanceAggregationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'ec2InstanceAggregationResponse_accountId' - The Amazon Web Services account for the Amazon EC2 instance.
--
-- 'ami', 'ec2InstanceAggregationResponse_ami' - The Amazon Machine Image (AMI) of the Amazon EC2 instance.
--
-- 'instanceTags', 'ec2InstanceAggregationResponse_instanceTags' - The tags attached to the instance.
--
-- 'networkFindings', 'ec2InstanceAggregationResponse_networkFindings' - The number of network findings for the Amazon EC2 instance.
--
-- 'operatingSystem', 'ec2InstanceAggregationResponse_operatingSystem' - The operating system of the Amazon EC2 instance.
--
-- 'severityCounts', 'ec2InstanceAggregationResponse_severityCounts' - An object that contains the count of matched findings per severity.
--
-- 'instanceId', 'ec2InstanceAggregationResponse_instanceId' - The Amazon EC2 instance ID.
newEc2InstanceAggregationResponse ::
  -- | 'instanceId'
  Prelude.Text ->
  Ec2InstanceAggregationResponse
newEc2InstanceAggregationResponse pInstanceId_ =
  Ec2InstanceAggregationResponse'
    { accountId =
        Prelude.Nothing,
      ami = Prelude.Nothing,
      instanceTags = Prelude.Nothing,
      networkFindings = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      severityCounts = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The Amazon Web Services account for the Amazon EC2 instance.
ec2InstanceAggregationResponse_accountId :: Lens.Lens' Ec2InstanceAggregationResponse (Prelude.Maybe Prelude.Text)
ec2InstanceAggregationResponse_accountId = Lens.lens (\Ec2InstanceAggregationResponse' {accountId} -> accountId) (\s@Ec2InstanceAggregationResponse' {} a -> s {accountId = a} :: Ec2InstanceAggregationResponse)

-- | The Amazon Machine Image (AMI) of the Amazon EC2 instance.
ec2InstanceAggregationResponse_ami :: Lens.Lens' Ec2InstanceAggregationResponse (Prelude.Maybe Prelude.Text)
ec2InstanceAggregationResponse_ami = Lens.lens (\Ec2InstanceAggregationResponse' {ami} -> ami) (\s@Ec2InstanceAggregationResponse' {} a -> s {ami = a} :: Ec2InstanceAggregationResponse)

-- | The tags attached to the instance.
ec2InstanceAggregationResponse_instanceTags :: Lens.Lens' Ec2InstanceAggregationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
ec2InstanceAggregationResponse_instanceTags = Lens.lens (\Ec2InstanceAggregationResponse' {instanceTags} -> instanceTags) (\s@Ec2InstanceAggregationResponse' {} a -> s {instanceTags = a} :: Ec2InstanceAggregationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The number of network findings for the Amazon EC2 instance.
ec2InstanceAggregationResponse_networkFindings :: Lens.Lens' Ec2InstanceAggregationResponse (Prelude.Maybe Prelude.Integer)
ec2InstanceAggregationResponse_networkFindings = Lens.lens (\Ec2InstanceAggregationResponse' {networkFindings} -> networkFindings) (\s@Ec2InstanceAggregationResponse' {} a -> s {networkFindings = a} :: Ec2InstanceAggregationResponse)

-- | The operating system of the Amazon EC2 instance.
ec2InstanceAggregationResponse_operatingSystem :: Lens.Lens' Ec2InstanceAggregationResponse (Prelude.Maybe Prelude.Text)
ec2InstanceAggregationResponse_operatingSystem = Lens.lens (\Ec2InstanceAggregationResponse' {operatingSystem} -> operatingSystem) (\s@Ec2InstanceAggregationResponse' {} a -> s {operatingSystem = a} :: Ec2InstanceAggregationResponse)

-- | An object that contains the count of matched findings per severity.
ec2InstanceAggregationResponse_severityCounts :: Lens.Lens' Ec2InstanceAggregationResponse (Prelude.Maybe SeverityCounts)
ec2InstanceAggregationResponse_severityCounts = Lens.lens (\Ec2InstanceAggregationResponse' {severityCounts} -> severityCounts) (\s@Ec2InstanceAggregationResponse' {} a -> s {severityCounts = a} :: Ec2InstanceAggregationResponse)

-- | The Amazon EC2 instance ID.
ec2InstanceAggregationResponse_instanceId :: Lens.Lens' Ec2InstanceAggregationResponse Prelude.Text
ec2InstanceAggregationResponse_instanceId = Lens.lens (\Ec2InstanceAggregationResponse' {instanceId} -> instanceId) (\s@Ec2InstanceAggregationResponse' {} a -> s {instanceId = a} :: Ec2InstanceAggregationResponse)

instance Data.FromJSON Ec2InstanceAggregationResponse where
  parseJSON =
    Data.withObject
      "Ec2InstanceAggregationResponse"
      ( \x ->
          Ec2InstanceAggregationResponse'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "ami")
            Prelude.<*> (x Data..:? "instanceTags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "networkFindings")
            Prelude.<*> (x Data..:? "operatingSystem")
            Prelude.<*> (x Data..:? "severityCounts")
            Prelude.<*> (x Data..: "instanceId")
      )

instance
  Prelude.Hashable
    Ec2InstanceAggregationResponse
  where
  hashWithSalt
    _salt
    Ec2InstanceAggregationResponse' {..} =
      _salt
        `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` ami
        `Prelude.hashWithSalt` instanceTags
        `Prelude.hashWithSalt` networkFindings
        `Prelude.hashWithSalt` operatingSystem
        `Prelude.hashWithSalt` severityCounts
        `Prelude.hashWithSalt` instanceId

instance
  Prelude.NFData
    Ec2InstanceAggregationResponse
  where
  rnf Ec2InstanceAggregationResponse' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf ami
      `Prelude.seq` Prelude.rnf instanceTags
      `Prelude.seq` Prelude.rnf networkFindings
      `Prelude.seq` Prelude.rnf operatingSystem
      `Prelude.seq` Prelude.rnf severityCounts
      `Prelude.seq` Prelude.rnf instanceId

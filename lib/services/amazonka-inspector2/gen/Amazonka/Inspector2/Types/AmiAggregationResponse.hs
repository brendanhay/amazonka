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
-- Module      : Amazonka.Inspector2.Types.AmiAggregationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AmiAggregationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.SeverityCounts
import qualified Amazonka.Prelude as Prelude

-- | A response that contains the results of a finding aggregation by AMI.
--
-- /See:/ 'newAmiAggregationResponse' smart constructor.
data AmiAggregationResponse = AmiAggregationResponse'
  { -- | The Amazon Web Services account ID for the AMI.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The IDs of Amazon EC2 instances using this AMI.
    affectedInstances :: Prelude.Maybe Prelude.Integer,
    -- | An object that contains the count of matched findings per severity.
    severityCounts :: Prelude.Maybe SeverityCounts,
    -- | The ID of the AMI that findings were aggregated for.
    ami :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmiAggregationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'amiAggregationResponse_accountId' - The Amazon Web Services account ID for the AMI.
--
-- 'affectedInstances', 'amiAggregationResponse_affectedInstances' - The IDs of Amazon EC2 instances using this AMI.
--
-- 'severityCounts', 'amiAggregationResponse_severityCounts' - An object that contains the count of matched findings per severity.
--
-- 'ami', 'amiAggregationResponse_ami' - The ID of the AMI that findings were aggregated for.
newAmiAggregationResponse ::
  -- | 'ami'
  Prelude.Text ->
  AmiAggregationResponse
newAmiAggregationResponse pAmi_ =
  AmiAggregationResponse'
    { accountId =
        Prelude.Nothing,
      affectedInstances = Prelude.Nothing,
      severityCounts = Prelude.Nothing,
      ami = pAmi_
    }

-- | The Amazon Web Services account ID for the AMI.
amiAggregationResponse_accountId :: Lens.Lens' AmiAggregationResponse (Prelude.Maybe Prelude.Text)
amiAggregationResponse_accountId = Lens.lens (\AmiAggregationResponse' {accountId} -> accountId) (\s@AmiAggregationResponse' {} a -> s {accountId = a} :: AmiAggregationResponse)

-- | The IDs of Amazon EC2 instances using this AMI.
amiAggregationResponse_affectedInstances :: Lens.Lens' AmiAggregationResponse (Prelude.Maybe Prelude.Integer)
amiAggregationResponse_affectedInstances = Lens.lens (\AmiAggregationResponse' {affectedInstances} -> affectedInstances) (\s@AmiAggregationResponse' {} a -> s {affectedInstances = a} :: AmiAggregationResponse)

-- | An object that contains the count of matched findings per severity.
amiAggregationResponse_severityCounts :: Lens.Lens' AmiAggregationResponse (Prelude.Maybe SeverityCounts)
amiAggregationResponse_severityCounts = Lens.lens (\AmiAggregationResponse' {severityCounts} -> severityCounts) (\s@AmiAggregationResponse' {} a -> s {severityCounts = a} :: AmiAggregationResponse)

-- | The ID of the AMI that findings were aggregated for.
amiAggregationResponse_ami :: Lens.Lens' AmiAggregationResponse Prelude.Text
amiAggregationResponse_ami = Lens.lens (\AmiAggregationResponse' {ami} -> ami) (\s@AmiAggregationResponse' {} a -> s {ami = a} :: AmiAggregationResponse)

instance Data.FromJSON AmiAggregationResponse where
  parseJSON =
    Data.withObject
      "AmiAggregationResponse"
      ( \x ->
          AmiAggregationResponse'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "affectedInstances")
            Prelude.<*> (x Data..:? "severityCounts")
            Prelude.<*> (x Data..: "ami")
      )

instance Prelude.Hashable AmiAggregationResponse where
  hashWithSalt _salt AmiAggregationResponse' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` affectedInstances
      `Prelude.hashWithSalt` severityCounts
      `Prelude.hashWithSalt` ami

instance Prelude.NFData AmiAggregationResponse where
  rnf AmiAggregationResponse' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf affectedInstances
      `Prelude.seq` Prelude.rnf severityCounts
      `Prelude.seq` Prelude.rnf ami

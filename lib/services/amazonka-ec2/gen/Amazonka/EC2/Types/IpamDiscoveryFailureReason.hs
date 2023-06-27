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
-- Module      : Amazonka.EC2.Types.IpamDiscoveryFailureReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamDiscoveryFailureReason where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.IpamDiscoveryFailureCode
import qualified Amazonka.Prelude as Prelude

-- | The discovery failure reason.
--
-- /See:/ 'newIpamDiscoveryFailureReason' smart constructor.
data IpamDiscoveryFailureReason = IpamDiscoveryFailureReason'
  { -- | The discovery failure code.
    --
    -- -   @assume-role-failure@ - IPAM could not assume the Amazon Web
    --     Services IAM service-linked role. This could be because of any of
    --     the following:
    --
    --     -   SLR has not been created yet and IPAM is still creating it.
    --
    --     -   You have opted-out of the IPAM home Region.
    --
    --     -   Account you are using as your IPAM account has been suspended.
    --
    -- -   @throttling-failure@ - IPAM account is already using the allotted
    --     transactions per second and IPAM is receiving a throttling error
    --     when assuming the Amazon Web Services IAM SLR.
    --
    -- -   @unauthorized-failure@ - Amazon Web Services account making the
    --     request is not authorized. For more information, see
    --     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html AuthFailure>
    --     in the /Amazon Elastic Compute Cloud API Reference/.
    code :: Prelude.Maybe IpamDiscoveryFailureCode,
    -- | The discovery failure message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpamDiscoveryFailureReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'ipamDiscoveryFailureReason_code' - The discovery failure code.
--
-- -   @assume-role-failure@ - IPAM could not assume the Amazon Web
--     Services IAM service-linked role. This could be because of any of
--     the following:
--
--     -   SLR has not been created yet and IPAM is still creating it.
--
--     -   You have opted-out of the IPAM home Region.
--
--     -   Account you are using as your IPAM account has been suspended.
--
-- -   @throttling-failure@ - IPAM account is already using the allotted
--     transactions per second and IPAM is receiving a throttling error
--     when assuming the Amazon Web Services IAM SLR.
--
-- -   @unauthorized-failure@ - Amazon Web Services account making the
--     request is not authorized. For more information, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html AuthFailure>
--     in the /Amazon Elastic Compute Cloud API Reference/.
--
-- 'message', 'ipamDiscoveryFailureReason_message' - The discovery failure message.
newIpamDiscoveryFailureReason ::
  IpamDiscoveryFailureReason
newIpamDiscoveryFailureReason =
  IpamDiscoveryFailureReason'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The discovery failure code.
--
-- -   @assume-role-failure@ - IPAM could not assume the Amazon Web
--     Services IAM service-linked role. This could be because of any of
--     the following:
--
--     -   SLR has not been created yet and IPAM is still creating it.
--
--     -   You have opted-out of the IPAM home Region.
--
--     -   Account you are using as your IPAM account has been suspended.
--
-- -   @throttling-failure@ - IPAM account is already using the allotted
--     transactions per second and IPAM is receiving a throttling error
--     when assuming the Amazon Web Services IAM SLR.
--
-- -   @unauthorized-failure@ - Amazon Web Services account making the
--     request is not authorized. For more information, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html AuthFailure>
--     in the /Amazon Elastic Compute Cloud API Reference/.
ipamDiscoveryFailureReason_code :: Lens.Lens' IpamDiscoveryFailureReason (Prelude.Maybe IpamDiscoveryFailureCode)
ipamDiscoveryFailureReason_code = Lens.lens (\IpamDiscoveryFailureReason' {code} -> code) (\s@IpamDiscoveryFailureReason' {} a -> s {code = a} :: IpamDiscoveryFailureReason)

-- | The discovery failure message.
ipamDiscoveryFailureReason_message :: Lens.Lens' IpamDiscoveryFailureReason (Prelude.Maybe Prelude.Text)
ipamDiscoveryFailureReason_message = Lens.lens (\IpamDiscoveryFailureReason' {message} -> message) (\s@IpamDiscoveryFailureReason' {} a -> s {message = a} :: IpamDiscoveryFailureReason)

instance Data.FromXML IpamDiscoveryFailureReason where
  parseXML x =
    IpamDiscoveryFailureReason'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")

instance Prelude.Hashable IpamDiscoveryFailureReason where
  hashWithSalt _salt IpamDiscoveryFailureReason' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData IpamDiscoveryFailureReason where
  rnf IpamDiscoveryFailureReason' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message

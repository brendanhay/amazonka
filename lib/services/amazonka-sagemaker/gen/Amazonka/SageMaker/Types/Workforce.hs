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
-- Module      : Amazonka.SageMaker.Types.Workforce
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Workforce where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CognitoConfig
import Amazonka.SageMaker.Types.OidcConfigForResponse
import Amazonka.SageMaker.Types.SourceIpConfig
import Amazonka.SageMaker.Types.WorkforceStatus
import Amazonka.SageMaker.Types.WorkforceVpcConfigResponse

-- | A single private workforce, which is automatically created when you
-- create your first private work team. You can create one private work
-- force in each Amazon Web Services Region. By default, any
-- workforce-related API operation used in a specific region will apply to
-- the workforce created in that region. To learn how to create a private
-- workforce, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce>.
--
-- /See:/ 'newWorkforce' smart constructor.
data Workforce = Workforce'
  { -- | The configuration of an Amazon Cognito workforce. A single Cognito
    -- workforce is created using and corresponds to a single
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool>.
    cognitoConfig :: Prelude.Maybe CognitoConfig,
    -- | The date that the workforce is created.
    createDate :: Prelude.Maybe Data.POSIX,
    -- | The reason your workforce failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The most recent date that
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_UpdateWorkforce.html UpdateWorkforce>
    -- was used to successfully add one or more IP address ranges
    -- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
    -- to a private workforce\'s allow list.
    lastUpdatedDate :: Prelude.Maybe Data.POSIX,
    -- | The configuration of an OIDC Identity Provider (IdP) private workforce.
    oidcConfig :: Prelude.Maybe OidcConfigForResponse,
    -- | A list of one to ten IP address ranges
    -- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
    -- to be added to the workforce allow list. By default, a workforce isn\'t
    -- restricted to specific IP addresses.
    sourceIpConfig :: Prelude.Maybe SourceIpConfig,
    -- | The status of your workforce.
    status :: Prelude.Maybe WorkforceStatus,
    -- | The subdomain for your OIDC Identity Provider.
    subDomain :: Prelude.Maybe Prelude.Text,
    -- | The configuration of a VPC workforce.
    workforceVpcConfig :: Prelude.Maybe WorkforceVpcConfigResponse,
    -- | The name of the private workforce.
    workforceName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the private workforce.
    workforceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Workforce' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cognitoConfig', 'workforce_cognitoConfig' - The configuration of an Amazon Cognito workforce. A single Cognito
-- workforce is created using and corresponds to a single
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool>.
--
-- 'createDate', 'workforce_createDate' - The date that the workforce is created.
--
-- 'failureReason', 'workforce_failureReason' - The reason your workforce failed.
--
-- 'lastUpdatedDate', 'workforce_lastUpdatedDate' - The most recent date that
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_UpdateWorkforce.html UpdateWorkforce>
-- was used to successfully add one or more IP address ranges
-- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
-- to a private workforce\'s allow list.
--
-- 'oidcConfig', 'workforce_oidcConfig' - The configuration of an OIDC Identity Provider (IdP) private workforce.
--
-- 'sourceIpConfig', 'workforce_sourceIpConfig' - A list of one to ten IP address ranges
-- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
-- to be added to the workforce allow list. By default, a workforce isn\'t
-- restricted to specific IP addresses.
--
-- 'status', 'workforce_status' - The status of your workforce.
--
-- 'subDomain', 'workforce_subDomain' - The subdomain for your OIDC Identity Provider.
--
-- 'workforceVpcConfig', 'workforce_workforceVpcConfig' - The configuration of a VPC workforce.
--
-- 'workforceName', 'workforce_workforceName' - The name of the private workforce.
--
-- 'workforceArn', 'workforce_workforceArn' - The Amazon Resource Name (ARN) of the private workforce.
newWorkforce ::
  -- | 'workforceName'
  Prelude.Text ->
  -- | 'workforceArn'
  Prelude.Text ->
  Workforce
newWorkforce pWorkforceName_ pWorkforceArn_ =
  Workforce'
    { cognitoConfig = Prelude.Nothing,
      createDate = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      oidcConfig = Prelude.Nothing,
      sourceIpConfig = Prelude.Nothing,
      status = Prelude.Nothing,
      subDomain = Prelude.Nothing,
      workforceVpcConfig = Prelude.Nothing,
      workforceName = pWorkforceName_,
      workforceArn = pWorkforceArn_
    }

-- | The configuration of an Amazon Cognito workforce. A single Cognito
-- workforce is created using and corresponds to a single
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool>.
workforce_cognitoConfig :: Lens.Lens' Workforce (Prelude.Maybe CognitoConfig)
workforce_cognitoConfig = Lens.lens (\Workforce' {cognitoConfig} -> cognitoConfig) (\s@Workforce' {} a -> s {cognitoConfig = a} :: Workforce)

-- | The date that the workforce is created.
workforce_createDate :: Lens.Lens' Workforce (Prelude.Maybe Prelude.UTCTime)
workforce_createDate = Lens.lens (\Workforce' {createDate} -> createDate) (\s@Workforce' {} a -> s {createDate = a} :: Workforce) Prelude.. Lens.mapping Data._Time

-- | The reason your workforce failed.
workforce_failureReason :: Lens.Lens' Workforce (Prelude.Maybe Prelude.Text)
workforce_failureReason = Lens.lens (\Workforce' {failureReason} -> failureReason) (\s@Workforce' {} a -> s {failureReason = a} :: Workforce)

-- | The most recent date that
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_UpdateWorkforce.html UpdateWorkforce>
-- was used to successfully add one or more IP address ranges
-- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
-- to a private workforce\'s allow list.
workforce_lastUpdatedDate :: Lens.Lens' Workforce (Prelude.Maybe Prelude.UTCTime)
workforce_lastUpdatedDate = Lens.lens (\Workforce' {lastUpdatedDate} -> lastUpdatedDate) (\s@Workforce' {} a -> s {lastUpdatedDate = a} :: Workforce) Prelude.. Lens.mapping Data._Time

-- | The configuration of an OIDC Identity Provider (IdP) private workforce.
workforce_oidcConfig :: Lens.Lens' Workforce (Prelude.Maybe OidcConfigForResponse)
workforce_oidcConfig = Lens.lens (\Workforce' {oidcConfig} -> oidcConfig) (\s@Workforce' {} a -> s {oidcConfig = a} :: Workforce)

-- | A list of one to ten IP address ranges
-- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
-- to be added to the workforce allow list. By default, a workforce isn\'t
-- restricted to specific IP addresses.
workforce_sourceIpConfig :: Lens.Lens' Workforce (Prelude.Maybe SourceIpConfig)
workforce_sourceIpConfig = Lens.lens (\Workforce' {sourceIpConfig} -> sourceIpConfig) (\s@Workforce' {} a -> s {sourceIpConfig = a} :: Workforce)

-- | The status of your workforce.
workforce_status :: Lens.Lens' Workforce (Prelude.Maybe WorkforceStatus)
workforce_status = Lens.lens (\Workforce' {status} -> status) (\s@Workforce' {} a -> s {status = a} :: Workforce)

-- | The subdomain for your OIDC Identity Provider.
workforce_subDomain :: Lens.Lens' Workforce (Prelude.Maybe Prelude.Text)
workforce_subDomain = Lens.lens (\Workforce' {subDomain} -> subDomain) (\s@Workforce' {} a -> s {subDomain = a} :: Workforce)

-- | The configuration of a VPC workforce.
workforce_workforceVpcConfig :: Lens.Lens' Workforce (Prelude.Maybe WorkforceVpcConfigResponse)
workforce_workforceVpcConfig = Lens.lens (\Workforce' {workforceVpcConfig} -> workforceVpcConfig) (\s@Workforce' {} a -> s {workforceVpcConfig = a} :: Workforce)

-- | The name of the private workforce.
workforce_workforceName :: Lens.Lens' Workforce Prelude.Text
workforce_workforceName = Lens.lens (\Workforce' {workforceName} -> workforceName) (\s@Workforce' {} a -> s {workforceName = a} :: Workforce)

-- | The Amazon Resource Name (ARN) of the private workforce.
workforce_workforceArn :: Lens.Lens' Workforce Prelude.Text
workforce_workforceArn = Lens.lens (\Workforce' {workforceArn} -> workforceArn) (\s@Workforce' {} a -> s {workforceArn = a} :: Workforce)

instance Data.FromJSON Workforce where
  parseJSON =
    Data.withObject
      "Workforce"
      ( \x ->
          Workforce'
            Prelude.<$> (x Data..:? "CognitoConfig")
            Prelude.<*> (x Data..:? "CreateDate")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "LastUpdatedDate")
            Prelude.<*> (x Data..:? "OidcConfig")
            Prelude.<*> (x Data..:? "SourceIpConfig")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "SubDomain")
            Prelude.<*> (x Data..:? "WorkforceVpcConfig")
            Prelude.<*> (x Data..: "WorkforceName")
            Prelude.<*> (x Data..: "WorkforceArn")
      )

instance Prelude.Hashable Workforce where
  hashWithSalt _salt Workforce' {..} =
    _salt
      `Prelude.hashWithSalt` cognitoConfig
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` lastUpdatedDate
      `Prelude.hashWithSalt` oidcConfig
      `Prelude.hashWithSalt` sourceIpConfig
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` subDomain
      `Prelude.hashWithSalt` workforceVpcConfig
      `Prelude.hashWithSalt` workforceName
      `Prelude.hashWithSalt` workforceArn

instance Prelude.NFData Workforce where
  rnf Workforce' {..} =
    Prelude.rnf cognitoConfig
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf oidcConfig
      `Prelude.seq` Prelude.rnf sourceIpConfig
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf subDomain
      `Prelude.seq` Prelude.rnf workforceVpcConfig
      `Prelude.seq` Prelude.rnf workforceName
      `Prelude.seq` Prelude.rnf workforceArn

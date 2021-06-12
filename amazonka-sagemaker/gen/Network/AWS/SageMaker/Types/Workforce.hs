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
-- Module      : Network.AWS.SageMaker.Types.Workforce
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Workforce where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.CognitoConfig
import Network.AWS.SageMaker.Types.OidcConfigForResponse
import Network.AWS.SageMaker.Types.SourceIpConfig

-- | A single private workforce, which is automatically created when you
-- create your first private work team. You can create one private work
-- force in each AWS Region. By default, any workforce-related API
-- operation used in a specific region will apply to the workforce created
-- in that region. To learn how to create a private workforce, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce>.
--
-- /See:/ 'newWorkforce' smart constructor.
data Workforce = Workforce'
  { -- | The most recent date that was used to successfully add one or more IP
    -- address ranges
    -- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
    -- to a private workforce\'s allow list.
    lastUpdatedDate :: Core.Maybe Core.POSIX,
    -- | The date that the workforce is created.
    createDate :: Core.Maybe Core.POSIX,
    -- | The subdomain for your OIDC Identity Provider.
    subDomain :: Core.Maybe Core.Text,
    -- | A list of one to ten IP address ranges
    -- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
    -- to be added to the workforce allow list. By default, a workforce isn\'t
    -- restricted to specific IP addresses.
    sourceIpConfig :: Core.Maybe SourceIpConfig,
    -- | The configuration of an OIDC Identity Provider (IdP) private workforce.
    oidcConfig :: Core.Maybe OidcConfigForResponse,
    -- | The configuration of an Amazon Cognito workforce. A single Cognito
    -- workforce is created using and corresponds to a single
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool>.
    cognitoConfig :: Core.Maybe CognitoConfig,
    -- | The name of the private workforce.
    workforceName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the private workforce.
    workforceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Workforce' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedDate', 'workforce_lastUpdatedDate' - The most recent date that was used to successfully add one or more IP
-- address ranges
-- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
-- to a private workforce\'s allow list.
--
-- 'createDate', 'workforce_createDate' - The date that the workforce is created.
--
-- 'subDomain', 'workforce_subDomain' - The subdomain for your OIDC Identity Provider.
--
-- 'sourceIpConfig', 'workforce_sourceIpConfig' - A list of one to ten IP address ranges
-- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
-- to be added to the workforce allow list. By default, a workforce isn\'t
-- restricted to specific IP addresses.
--
-- 'oidcConfig', 'workforce_oidcConfig' - The configuration of an OIDC Identity Provider (IdP) private workforce.
--
-- 'cognitoConfig', 'workforce_cognitoConfig' - The configuration of an Amazon Cognito workforce. A single Cognito
-- workforce is created using and corresponds to a single
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool>.
--
-- 'workforceName', 'workforce_workforceName' - The name of the private workforce.
--
-- 'workforceArn', 'workforce_workforceArn' - The Amazon Resource Name (ARN) of the private workforce.
newWorkforce ::
  -- | 'workforceName'
  Core.Text ->
  -- | 'workforceArn'
  Core.Text ->
  Workforce
newWorkforce pWorkforceName_ pWorkforceArn_ =
  Workforce'
    { lastUpdatedDate = Core.Nothing,
      createDate = Core.Nothing,
      subDomain = Core.Nothing,
      sourceIpConfig = Core.Nothing,
      oidcConfig = Core.Nothing,
      cognitoConfig = Core.Nothing,
      workforceName = pWorkforceName_,
      workforceArn = pWorkforceArn_
    }

-- | The most recent date that was used to successfully add one or more IP
-- address ranges
-- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
-- to a private workforce\'s allow list.
workforce_lastUpdatedDate :: Lens.Lens' Workforce (Core.Maybe Core.UTCTime)
workforce_lastUpdatedDate = Lens.lens (\Workforce' {lastUpdatedDate} -> lastUpdatedDate) (\s@Workforce' {} a -> s {lastUpdatedDate = a} :: Workforce) Core.. Lens.mapping Core._Time

-- | The date that the workforce is created.
workforce_createDate :: Lens.Lens' Workforce (Core.Maybe Core.UTCTime)
workforce_createDate = Lens.lens (\Workforce' {createDate} -> createDate) (\s@Workforce' {} a -> s {createDate = a} :: Workforce) Core.. Lens.mapping Core._Time

-- | The subdomain for your OIDC Identity Provider.
workforce_subDomain :: Lens.Lens' Workforce (Core.Maybe Core.Text)
workforce_subDomain = Lens.lens (\Workforce' {subDomain} -> subDomain) (\s@Workforce' {} a -> s {subDomain = a} :: Workforce)

-- | A list of one to ten IP address ranges
-- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
-- to be added to the workforce allow list. By default, a workforce isn\'t
-- restricted to specific IP addresses.
workforce_sourceIpConfig :: Lens.Lens' Workforce (Core.Maybe SourceIpConfig)
workforce_sourceIpConfig = Lens.lens (\Workforce' {sourceIpConfig} -> sourceIpConfig) (\s@Workforce' {} a -> s {sourceIpConfig = a} :: Workforce)

-- | The configuration of an OIDC Identity Provider (IdP) private workforce.
workforce_oidcConfig :: Lens.Lens' Workforce (Core.Maybe OidcConfigForResponse)
workforce_oidcConfig = Lens.lens (\Workforce' {oidcConfig} -> oidcConfig) (\s@Workforce' {} a -> s {oidcConfig = a} :: Workforce)

-- | The configuration of an Amazon Cognito workforce. A single Cognito
-- workforce is created using and corresponds to a single
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool>.
workforce_cognitoConfig :: Lens.Lens' Workforce (Core.Maybe CognitoConfig)
workforce_cognitoConfig = Lens.lens (\Workforce' {cognitoConfig} -> cognitoConfig) (\s@Workforce' {} a -> s {cognitoConfig = a} :: Workforce)

-- | The name of the private workforce.
workforce_workforceName :: Lens.Lens' Workforce Core.Text
workforce_workforceName = Lens.lens (\Workforce' {workforceName} -> workforceName) (\s@Workforce' {} a -> s {workforceName = a} :: Workforce)

-- | The Amazon Resource Name (ARN) of the private workforce.
workforce_workforceArn :: Lens.Lens' Workforce Core.Text
workforce_workforceArn = Lens.lens (\Workforce' {workforceArn} -> workforceArn) (\s@Workforce' {} a -> s {workforceArn = a} :: Workforce)

instance Core.FromJSON Workforce where
  parseJSON =
    Core.withObject
      "Workforce"
      ( \x ->
          Workforce'
            Core.<$> (x Core..:? "LastUpdatedDate")
            Core.<*> (x Core..:? "CreateDate")
            Core.<*> (x Core..:? "SubDomain")
            Core.<*> (x Core..:? "SourceIpConfig")
            Core.<*> (x Core..:? "OidcConfig")
            Core.<*> (x Core..:? "CognitoConfig")
            Core.<*> (x Core..: "WorkforceName")
            Core.<*> (x Core..: "WorkforceArn")
      )

instance Core.Hashable Workforce

instance Core.NFData Workforce

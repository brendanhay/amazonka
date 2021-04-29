{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    lastUpdatedDate :: Prelude.Maybe Prelude.POSIX,
    -- | The date that the workforce is created.
    createDate :: Prelude.Maybe Prelude.POSIX,
    -- | The subdomain for your OIDC Identity Provider.
    subDomain :: Prelude.Maybe Prelude.Text,
    -- | A list of one to ten IP address ranges
    -- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
    -- to be added to the workforce allow list. By default, a workforce isn\'t
    -- restricted to specific IP addresses.
    sourceIpConfig :: Prelude.Maybe SourceIpConfig,
    -- | The configuration of an OIDC Identity Provider (IdP) private workforce.
    oidcConfig :: Prelude.Maybe OidcConfigForResponse,
    -- | The configuration of an Amazon Cognito workforce. A single Cognito
    -- workforce is created using and corresponds to a single
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool>.
    cognitoConfig :: Prelude.Maybe CognitoConfig,
    -- | The name of the private workforce.
    workforceName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the private workforce.
    workforceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'workforceArn'
  Prelude.Text ->
  Workforce
newWorkforce pWorkforceName_ pWorkforceArn_ =
  Workforce'
    { lastUpdatedDate = Prelude.Nothing,
      createDate = Prelude.Nothing,
      subDomain = Prelude.Nothing,
      sourceIpConfig = Prelude.Nothing,
      oidcConfig = Prelude.Nothing,
      cognitoConfig = Prelude.Nothing,
      workforceName = pWorkforceName_,
      workforceArn = pWorkforceArn_
    }

-- | The most recent date that was used to successfully add one or more IP
-- address ranges
-- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
-- to a private workforce\'s allow list.
workforce_lastUpdatedDate :: Lens.Lens' Workforce (Prelude.Maybe Prelude.UTCTime)
workforce_lastUpdatedDate = Lens.lens (\Workforce' {lastUpdatedDate} -> lastUpdatedDate) (\s@Workforce' {} a -> s {lastUpdatedDate = a} :: Workforce) Prelude.. Lens.mapping Prelude._Time

-- | The date that the workforce is created.
workforce_createDate :: Lens.Lens' Workforce (Prelude.Maybe Prelude.UTCTime)
workforce_createDate = Lens.lens (\Workforce' {createDate} -> createDate) (\s@Workforce' {} a -> s {createDate = a} :: Workforce) Prelude.. Lens.mapping Prelude._Time

-- | The subdomain for your OIDC Identity Provider.
workforce_subDomain :: Lens.Lens' Workforce (Prelude.Maybe Prelude.Text)
workforce_subDomain = Lens.lens (\Workforce' {subDomain} -> subDomain) (\s@Workforce' {} a -> s {subDomain = a} :: Workforce)

-- | A list of one to ten IP address ranges
-- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>)
-- to be added to the workforce allow list. By default, a workforce isn\'t
-- restricted to specific IP addresses.
workforce_sourceIpConfig :: Lens.Lens' Workforce (Prelude.Maybe SourceIpConfig)
workforce_sourceIpConfig = Lens.lens (\Workforce' {sourceIpConfig} -> sourceIpConfig) (\s@Workforce' {} a -> s {sourceIpConfig = a} :: Workforce)

-- | The configuration of an OIDC Identity Provider (IdP) private workforce.
workforce_oidcConfig :: Lens.Lens' Workforce (Prelude.Maybe OidcConfigForResponse)
workforce_oidcConfig = Lens.lens (\Workforce' {oidcConfig} -> oidcConfig) (\s@Workforce' {} a -> s {oidcConfig = a} :: Workforce)

-- | The configuration of an Amazon Cognito workforce. A single Cognito
-- workforce is created using and corresponds to a single
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool>.
workforce_cognitoConfig :: Lens.Lens' Workforce (Prelude.Maybe CognitoConfig)
workforce_cognitoConfig = Lens.lens (\Workforce' {cognitoConfig} -> cognitoConfig) (\s@Workforce' {} a -> s {cognitoConfig = a} :: Workforce)

-- | The name of the private workforce.
workforce_workforceName :: Lens.Lens' Workforce Prelude.Text
workforce_workforceName = Lens.lens (\Workforce' {workforceName} -> workforceName) (\s@Workforce' {} a -> s {workforceName = a} :: Workforce)

-- | The Amazon Resource Name (ARN) of the private workforce.
workforce_workforceArn :: Lens.Lens' Workforce Prelude.Text
workforce_workforceArn = Lens.lens (\Workforce' {workforceArn} -> workforceArn) (\s@Workforce' {} a -> s {workforceArn = a} :: Workforce)

instance Prelude.FromJSON Workforce where
  parseJSON =
    Prelude.withObject
      "Workforce"
      ( \x ->
          Workforce'
            Prelude.<$> (x Prelude..:? "LastUpdatedDate")
            Prelude.<*> (x Prelude..:? "CreateDate")
            Prelude.<*> (x Prelude..:? "SubDomain")
            Prelude.<*> (x Prelude..:? "SourceIpConfig")
            Prelude.<*> (x Prelude..:? "OidcConfig")
            Prelude.<*> (x Prelude..:? "CognitoConfig")
            Prelude.<*> (x Prelude..: "WorkforceName")
            Prelude.<*> (x Prelude..: "WorkforceArn")
      )

instance Prelude.Hashable Workforce

instance Prelude.NFData Workforce

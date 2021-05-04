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
-- Module      : Network.AWS.SageMaker.Types.Workteam
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Workteam where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.MemberDefinition
import Network.AWS.SageMaker.Types.NotificationConfiguration

-- | Provides details about a labeling work team.
--
-- /See:/ 'newWorkteam' smart constructor.
data Workteam = Workteam'
  { -- | The Amazon Resource Name (ARN) of the workforce.
    workforceArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the work team was last updated (timestamp).
    lastUpdatedDate :: Prelude.Maybe Prelude.POSIX,
    -- | The date and time that the work team was created (timestamp).
    createDate :: Prelude.Maybe Prelude.POSIX,
    -- | Configures SNS notifications of available or expiring work items for
    -- work teams.
    notificationConfiguration :: Prelude.Maybe NotificationConfiguration,
    -- | The Amazon Marketplace identifier for a vendor\'s work team.
    productListingIds :: Prelude.Maybe [Prelude.Text],
    -- | The URI of the labeling job\'s user interface. Workers open this URI to
    -- start labeling your data objects.
    subDomain :: Prelude.Maybe Prelude.Text,
    -- | The name of the work team.
    workteamName :: Prelude.Text,
    -- | A list of @MemberDefinition@ objects that contains objects that identify
    -- the workers that make up the work team.
    --
    -- Workforces can be created using Amazon Cognito or your own OIDC Identity
    -- Provider (IdP). For private workforces created using Amazon Cognito use
    -- @CognitoMemberDefinition@. For workforces created using your own OIDC
    -- identity provider (IdP) use @OidcMemberDefinition@.
    memberDefinitions :: Prelude.NonEmpty MemberDefinition,
    -- | The Amazon Resource Name (ARN) that identifies the work team.
    workteamArn :: Prelude.Text,
    -- | A description of the work team.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Workteam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workforceArn', 'workteam_workforceArn' - The Amazon Resource Name (ARN) of the workforce.
--
-- 'lastUpdatedDate', 'workteam_lastUpdatedDate' - The date and time that the work team was last updated (timestamp).
--
-- 'createDate', 'workteam_createDate' - The date and time that the work team was created (timestamp).
--
-- 'notificationConfiguration', 'workteam_notificationConfiguration' - Configures SNS notifications of available or expiring work items for
-- work teams.
--
-- 'productListingIds', 'workteam_productListingIds' - The Amazon Marketplace identifier for a vendor\'s work team.
--
-- 'subDomain', 'workteam_subDomain' - The URI of the labeling job\'s user interface. Workers open this URI to
-- start labeling your data objects.
--
-- 'workteamName', 'workteam_workteamName' - The name of the work team.
--
-- 'memberDefinitions', 'workteam_memberDefinitions' - A list of @MemberDefinition@ objects that contains objects that identify
-- the workers that make up the work team.
--
-- Workforces can be created using Amazon Cognito or your own OIDC Identity
-- Provider (IdP). For private workforces created using Amazon Cognito use
-- @CognitoMemberDefinition@. For workforces created using your own OIDC
-- identity provider (IdP) use @OidcMemberDefinition@.
--
-- 'workteamArn', 'workteam_workteamArn' - The Amazon Resource Name (ARN) that identifies the work team.
--
-- 'description', 'workteam_description' - A description of the work team.
newWorkteam ::
  -- | 'workteamName'
  Prelude.Text ->
  -- | 'memberDefinitions'
  Prelude.NonEmpty MemberDefinition ->
  -- | 'workteamArn'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  Workteam
newWorkteam
  pWorkteamName_
  pMemberDefinitions_
  pWorkteamArn_
  pDescription_ =
    Workteam'
      { workforceArn = Prelude.Nothing,
        lastUpdatedDate = Prelude.Nothing,
        createDate = Prelude.Nothing,
        notificationConfiguration = Prelude.Nothing,
        productListingIds = Prelude.Nothing,
        subDomain = Prelude.Nothing,
        workteamName = pWorkteamName_,
        memberDefinitions =
          Prelude._Coerce Lens.# pMemberDefinitions_,
        workteamArn = pWorkteamArn_,
        description = pDescription_
      }

-- | The Amazon Resource Name (ARN) of the workforce.
workteam_workforceArn :: Lens.Lens' Workteam (Prelude.Maybe Prelude.Text)
workteam_workforceArn = Lens.lens (\Workteam' {workforceArn} -> workforceArn) (\s@Workteam' {} a -> s {workforceArn = a} :: Workteam)

-- | The date and time that the work team was last updated (timestamp).
workteam_lastUpdatedDate :: Lens.Lens' Workteam (Prelude.Maybe Prelude.UTCTime)
workteam_lastUpdatedDate = Lens.lens (\Workteam' {lastUpdatedDate} -> lastUpdatedDate) (\s@Workteam' {} a -> s {lastUpdatedDate = a} :: Workteam) Prelude.. Lens.mapping Prelude._Time

-- | The date and time that the work team was created (timestamp).
workteam_createDate :: Lens.Lens' Workteam (Prelude.Maybe Prelude.UTCTime)
workteam_createDate = Lens.lens (\Workteam' {createDate} -> createDate) (\s@Workteam' {} a -> s {createDate = a} :: Workteam) Prelude.. Lens.mapping Prelude._Time

-- | Configures SNS notifications of available or expiring work items for
-- work teams.
workteam_notificationConfiguration :: Lens.Lens' Workteam (Prelude.Maybe NotificationConfiguration)
workteam_notificationConfiguration = Lens.lens (\Workteam' {notificationConfiguration} -> notificationConfiguration) (\s@Workteam' {} a -> s {notificationConfiguration = a} :: Workteam)

-- | The Amazon Marketplace identifier for a vendor\'s work team.
workteam_productListingIds :: Lens.Lens' Workteam (Prelude.Maybe [Prelude.Text])
workteam_productListingIds = Lens.lens (\Workteam' {productListingIds} -> productListingIds) (\s@Workteam' {} a -> s {productListingIds = a} :: Workteam) Prelude.. Lens.mapping Prelude._Coerce

-- | The URI of the labeling job\'s user interface. Workers open this URI to
-- start labeling your data objects.
workteam_subDomain :: Lens.Lens' Workteam (Prelude.Maybe Prelude.Text)
workteam_subDomain = Lens.lens (\Workteam' {subDomain} -> subDomain) (\s@Workteam' {} a -> s {subDomain = a} :: Workteam)

-- | The name of the work team.
workteam_workteamName :: Lens.Lens' Workteam Prelude.Text
workteam_workteamName = Lens.lens (\Workteam' {workteamName} -> workteamName) (\s@Workteam' {} a -> s {workteamName = a} :: Workteam)

-- | A list of @MemberDefinition@ objects that contains objects that identify
-- the workers that make up the work team.
--
-- Workforces can be created using Amazon Cognito or your own OIDC Identity
-- Provider (IdP). For private workforces created using Amazon Cognito use
-- @CognitoMemberDefinition@. For workforces created using your own OIDC
-- identity provider (IdP) use @OidcMemberDefinition@.
workteam_memberDefinitions :: Lens.Lens' Workteam (Prelude.NonEmpty MemberDefinition)
workteam_memberDefinitions = Lens.lens (\Workteam' {memberDefinitions} -> memberDefinitions) (\s@Workteam' {} a -> s {memberDefinitions = a} :: Workteam) Prelude.. Prelude._Coerce

-- | The Amazon Resource Name (ARN) that identifies the work team.
workteam_workteamArn :: Lens.Lens' Workteam Prelude.Text
workteam_workteamArn = Lens.lens (\Workteam' {workteamArn} -> workteamArn) (\s@Workteam' {} a -> s {workteamArn = a} :: Workteam)

-- | A description of the work team.
workteam_description :: Lens.Lens' Workteam Prelude.Text
workteam_description = Lens.lens (\Workteam' {description} -> description) (\s@Workteam' {} a -> s {description = a} :: Workteam)

instance Prelude.FromJSON Workteam where
  parseJSON =
    Prelude.withObject
      "Workteam"
      ( \x ->
          Workteam'
            Prelude.<$> (x Prelude..:? "WorkforceArn")
            Prelude.<*> (x Prelude..:? "LastUpdatedDate")
            Prelude.<*> (x Prelude..:? "CreateDate")
            Prelude.<*> (x Prelude..:? "NotificationConfiguration")
            Prelude.<*> ( x Prelude..:? "ProductListingIds"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "SubDomain")
            Prelude.<*> (x Prelude..: "WorkteamName")
            Prelude.<*> (x Prelude..: "MemberDefinitions")
            Prelude.<*> (x Prelude..: "WorkteamArn")
            Prelude.<*> (x Prelude..: "Description")
      )

instance Prelude.Hashable Workteam

instance Prelude.NFData Workteam

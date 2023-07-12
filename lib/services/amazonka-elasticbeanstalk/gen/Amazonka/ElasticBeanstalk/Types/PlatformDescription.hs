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
-- Module      : Amazonka.ElasticBeanstalk.Types.PlatformDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.PlatformDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types.CustomAmi
import Amazonka.ElasticBeanstalk.Types.PlatformFramework
import Amazonka.ElasticBeanstalk.Types.PlatformProgrammingLanguage
import Amazonka.ElasticBeanstalk.Types.PlatformStatus
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about a platform version.
--
-- /See:/ 'newPlatformDescription' smart constructor.
data PlatformDescription = PlatformDescription'
  { -- | The custom AMIs supported by the platform version.
    customAmiList :: Prelude.Maybe [CustomAmi],
    -- | The date when the platform version was created.
    dateCreated :: Prelude.Maybe Data.ISO8601,
    -- | The date when the platform version was last updated.
    dateUpdated :: Prelude.Maybe Data.ISO8601,
    -- | The description of the platform version.
    description :: Prelude.Maybe Prelude.Text,
    -- | The frameworks supported by the platform version.
    frameworks :: Prelude.Maybe [PlatformFramework],
    -- | Information about the maintainer of the platform version.
    maintainer :: Prelude.Maybe Prelude.Text,
    -- | The operating system used by the platform version.
    operatingSystemName :: Prelude.Maybe Prelude.Text,
    -- | The version of the operating system used by the platform version.
    operatingSystemVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the platform version.
    platformArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the platform version\'s branch in its lifecycle.
    --
    -- Possible values: @Beta@ | @Supported@ | @Deprecated@ | @Retired@
    platformBranchLifecycleState :: Prelude.Maybe Prelude.Text,
    -- | The platform branch to which the platform version belongs.
    platformBranchName :: Prelude.Maybe Prelude.Text,
    -- | The category of the platform version.
    platformCategory :: Prelude.Maybe Prelude.Text,
    -- | The state of the platform version in its lifecycle.
    --
    -- Possible values: @Recommended@ | @null@
    --
    -- If a null value is returned, the platform version isn\'t the recommended
    -- one for its branch. Each platform branch has a single recommended
    -- platform version, typically the most recent one.
    platformLifecycleState :: Prelude.Maybe Prelude.Text,
    -- | The name of the platform version.
    platformName :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID of the person who created the platform version.
    platformOwner :: Prelude.Maybe Prelude.Text,
    -- | The status of the platform version.
    platformStatus :: Prelude.Maybe PlatformStatus,
    -- | The version of the platform version.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | The programming languages supported by the platform version.
    programmingLanguages :: Prelude.Maybe [PlatformProgrammingLanguage],
    -- | The name of the solution stack used by the platform version.
    solutionStackName :: Prelude.Maybe Prelude.Text,
    -- | The additions supported by the platform version.
    supportedAddonList :: Prelude.Maybe [Prelude.Text],
    -- | The tiers supported by the platform version.
    supportedTierList :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlatformDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customAmiList', 'platformDescription_customAmiList' - The custom AMIs supported by the platform version.
--
-- 'dateCreated', 'platformDescription_dateCreated' - The date when the platform version was created.
--
-- 'dateUpdated', 'platformDescription_dateUpdated' - The date when the platform version was last updated.
--
-- 'description', 'platformDescription_description' - The description of the platform version.
--
-- 'frameworks', 'platformDescription_frameworks' - The frameworks supported by the platform version.
--
-- 'maintainer', 'platformDescription_maintainer' - Information about the maintainer of the platform version.
--
-- 'operatingSystemName', 'platformDescription_operatingSystemName' - The operating system used by the platform version.
--
-- 'operatingSystemVersion', 'platformDescription_operatingSystemVersion' - The version of the operating system used by the platform version.
--
-- 'platformArn', 'platformDescription_platformArn' - The ARN of the platform version.
--
-- 'platformBranchLifecycleState', 'platformDescription_platformBranchLifecycleState' - The state of the platform version\'s branch in its lifecycle.
--
-- Possible values: @Beta@ | @Supported@ | @Deprecated@ | @Retired@
--
-- 'platformBranchName', 'platformDescription_platformBranchName' - The platform branch to which the platform version belongs.
--
-- 'platformCategory', 'platformDescription_platformCategory' - The category of the platform version.
--
-- 'platformLifecycleState', 'platformDescription_platformLifecycleState' - The state of the platform version in its lifecycle.
--
-- Possible values: @Recommended@ | @null@
--
-- If a null value is returned, the platform version isn\'t the recommended
-- one for its branch. Each platform branch has a single recommended
-- platform version, typically the most recent one.
--
-- 'platformName', 'platformDescription_platformName' - The name of the platform version.
--
-- 'platformOwner', 'platformDescription_platformOwner' - The AWS account ID of the person who created the platform version.
--
-- 'platformStatus', 'platformDescription_platformStatus' - The status of the platform version.
--
-- 'platformVersion', 'platformDescription_platformVersion' - The version of the platform version.
--
-- 'programmingLanguages', 'platformDescription_programmingLanguages' - The programming languages supported by the platform version.
--
-- 'solutionStackName', 'platformDescription_solutionStackName' - The name of the solution stack used by the platform version.
--
-- 'supportedAddonList', 'platformDescription_supportedAddonList' - The additions supported by the platform version.
--
-- 'supportedTierList', 'platformDescription_supportedTierList' - The tiers supported by the platform version.
newPlatformDescription ::
  PlatformDescription
newPlatformDescription =
  PlatformDescription'
    { customAmiList =
        Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      dateUpdated = Prelude.Nothing,
      description = Prelude.Nothing,
      frameworks = Prelude.Nothing,
      maintainer = Prelude.Nothing,
      operatingSystemName = Prelude.Nothing,
      operatingSystemVersion = Prelude.Nothing,
      platformArn = Prelude.Nothing,
      platformBranchLifecycleState = Prelude.Nothing,
      platformBranchName = Prelude.Nothing,
      platformCategory = Prelude.Nothing,
      platformLifecycleState = Prelude.Nothing,
      platformName = Prelude.Nothing,
      platformOwner = Prelude.Nothing,
      platformStatus = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      programmingLanguages = Prelude.Nothing,
      solutionStackName = Prelude.Nothing,
      supportedAddonList = Prelude.Nothing,
      supportedTierList = Prelude.Nothing
    }

-- | The custom AMIs supported by the platform version.
platformDescription_customAmiList :: Lens.Lens' PlatformDescription (Prelude.Maybe [CustomAmi])
platformDescription_customAmiList = Lens.lens (\PlatformDescription' {customAmiList} -> customAmiList) (\s@PlatformDescription' {} a -> s {customAmiList = a} :: PlatformDescription) Prelude.. Lens.mapping Lens.coerced

-- | The date when the platform version was created.
platformDescription_dateCreated :: Lens.Lens' PlatformDescription (Prelude.Maybe Prelude.UTCTime)
platformDescription_dateCreated = Lens.lens (\PlatformDescription' {dateCreated} -> dateCreated) (\s@PlatformDescription' {} a -> s {dateCreated = a} :: PlatformDescription) Prelude.. Lens.mapping Data._Time

-- | The date when the platform version was last updated.
platformDescription_dateUpdated :: Lens.Lens' PlatformDescription (Prelude.Maybe Prelude.UTCTime)
platformDescription_dateUpdated = Lens.lens (\PlatformDescription' {dateUpdated} -> dateUpdated) (\s@PlatformDescription' {} a -> s {dateUpdated = a} :: PlatformDescription) Prelude.. Lens.mapping Data._Time

-- | The description of the platform version.
platformDescription_description :: Lens.Lens' PlatformDescription (Prelude.Maybe Prelude.Text)
platformDescription_description = Lens.lens (\PlatformDescription' {description} -> description) (\s@PlatformDescription' {} a -> s {description = a} :: PlatformDescription)

-- | The frameworks supported by the platform version.
platformDescription_frameworks :: Lens.Lens' PlatformDescription (Prelude.Maybe [PlatformFramework])
platformDescription_frameworks = Lens.lens (\PlatformDescription' {frameworks} -> frameworks) (\s@PlatformDescription' {} a -> s {frameworks = a} :: PlatformDescription) Prelude.. Lens.mapping Lens.coerced

-- | Information about the maintainer of the platform version.
platformDescription_maintainer :: Lens.Lens' PlatformDescription (Prelude.Maybe Prelude.Text)
platformDescription_maintainer = Lens.lens (\PlatformDescription' {maintainer} -> maintainer) (\s@PlatformDescription' {} a -> s {maintainer = a} :: PlatformDescription)

-- | The operating system used by the platform version.
platformDescription_operatingSystemName :: Lens.Lens' PlatformDescription (Prelude.Maybe Prelude.Text)
platformDescription_operatingSystemName = Lens.lens (\PlatformDescription' {operatingSystemName} -> operatingSystemName) (\s@PlatformDescription' {} a -> s {operatingSystemName = a} :: PlatformDescription)

-- | The version of the operating system used by the platform version.
platformDescription_operatingSystemVersion :: Lens.Lens' PlatformDescription (Prelude.Maybe Prelude.Text)
platformDescription_operatingSystemVersion = Lens.lens (\PlatformDescription' {operatingSystemVersion} -> operatingSystemVersion) (\s@PlatformDescription' {} a -> s {operatingSystemVersion = a} :: PlatformDescription)

-- | The ARN of the platform version.
platformDescription_platformArn :: Lens.Lens' PlatformDescription (Prelude.Maybe Prelude.Text)
platformDescription_platformArn = Lens.lens (\PlatformDescription' {platformArn} -> platformArn) (\s@PlatformDescription' {} a -> s {platformArn = a} :: PlatformDescription)

-- | The state of the platform version\'s branch in its lifecycle.
--
-- Possible values: @Beta@ | @Supported@ | @Deprecated@ | @Retired@
platformDescription_platformBranchLifecycleState :: Lens.Lens' PlatformDescription (Prelude.Maybe Prelude.Text)
platformDescription_platformBranchLifecycleState = Lens.lens (\PlatformDescription' {platformBranchLifecycleState} -> platformBranchLifecycleState) (\s@PlatformDescription' {} a -> s {platformBranchLifecycleState = a} :: PlatformDescription)

-- | The platform branch to which the platform version belongs.
platformDescription_platformBranchName :: Lens.Lens' PlatformDescription (Prelude.Maybe Prelude.Text)
platformDescription_platformBranchName = Lens.lens (\PlatformDescription' {platformBranchName} -> platformBranchName) (\s@PlatformDescription' {} a -> s {platformBranchName = a} :: PlatformDescription)

-- | The category of the platform version.
platformDescription_platformCategory :: Lens.Lens' PlatformDescription (Prelude.Maybe Prelude.Text)
platformDescription_platformCategory = Lens.lens (\PlatformDescription' {platformCategory} -> platformCategory) (\s@PlatformDescription' {} a -> s {platformCategory = a} :: PlatformDescription)

-- | The state of the platform version in its lifecycle.
--
-- Possible values: @Recommended@ | @null@
--
-- If a null value is returned, the platform version isn\'t the recommended
-- one for its branch. Each platform branch has a single recommended
-- platform version, typically the most recent one.
platformDescription_platformLifecycleState :: Lens.Lens' PlatformDescription (Prelude.Maybe Prelude.Text)
platformDescription_platformLifecycleState = Lens.lens (\PlatformDescription' {platformLifecycleState} -> platformLifecycleState) (\s@PlatformDescription' {} a -> s {platformLifecycleState = a} :: PlatformDescription)

-- | The name of the platform version.
platformDescription_platformName :: Lens.Lens' PlatformDescription (Prelude.Maybe Prelude.Text)
platformDescription_platformName = Lens.lens (\PlatformDescription' {platformName} -> platformName) (\s@PlatformDescription' {} a -> s {platformName = a} :: PlatformDescription)

-- | The AWS account ID of the person who created the platform version.
platformDescription_platformOwner :: Lens.Lens' PlatformDescription (Prelude.Maybe Prelude.Text)
platformDescription_platformOwner = Lens.lens (\PlatformDescription' {platformOwner} -> platformOwner) (\s@PlatformDescription' {} a -> s {platformOwner = a} :: PlatformDescription)

-- | The status of the platform version.
platformDescription_platformStatus :: Lens.Lens' PlatformDescription (Prelude.Maybe PlatformStatus)
platformDescription_platformStatus = Lens.lens (\PlatformDescription' {platformStatus} -> platformStatus) (\s@PlatformDescription' {} a -> s {platformStatus = a} :: PlatformDescription)

-- | The version of the platform version.
platformDescription_platformVersion :: Lens.Lens' PlatformDescription (Prelude.Maybe Prelude.Text)
platformDescription_platformVersion = Lens.lens (\PlatformDescription' {platformVersion} -> platformVersion) (\s@PlatformDescription' {} a -> s {platformVersion = a} :: PlatformDescription)

-- | The programming languages supported by the platform version.
platformDescription_programmingLanguages :: Lens.Lens' PlatformDescription (Prelude.Maybe [PlatformProgrammingLanguage])
platformDescription_programmingLanguages = Lens.lens (\PlatformDescription' {programmingLanguages} -> programmingLanguages) (\s@PlatformDescription' {} a -> s {programmingLanguages = a} :: PlatformDescription) Prelude.. Lens.mapping Lens.coerced

-- | The name of the solution stack used by the platform version.
platformDescription_solutionStackName :: Lens.Lens' PlatformDescription (Prelude.Maybe Prelude.Text)
platformDescription_solutionStackName = Lens.lens (\PlatformDescription' {solutionStackName} -> solutionStackName) (\s@PlatformDescription' {} a -> s {solutionStackName = a} :: PlatformDescription)

-- | The additions supported by the platform version.
platformDescription_supportedAddonList :: Lens.Lens' PlatformDescription (Prelude.Maybe [Prelude.Text])
platformDescription_supportedAddonList = Lens.lens (\PlatformDescription' {supportedAddonList} -> supportedAddonList) (\s@PlatformDescription' {} a -> s {supportedAddonList = a} :: PlatformDescription) Prelude.. Lens.mapping Lens.coerced

-- | The tiers supported by the platform version.
platformDescription_supportedTierList :: Lens.Lens' PlatformDescription (Prelude.Maybe [Prelude.Text])
platformDescription_supportedTierList = Lens.lens (\PlatformDescription' {supportedTierList} -> supportedTierList) (\s@PlatformDescription' {} a -> s {supportedTierList = a} :: PlatformDescription) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML PlatformDescription where
  parseXML x =
    PlatformDescription'
      Prelude.<$> ( x
                      Data..@? "CustomAmiList"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "DateCreated")
      Prelude.<*> (x Data..@? "DateUpdated")
      Prelude.<*> (x Data..@? "Description")
      Prelude.<*> ( x
                      Data..@? "Frameworks"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "Maintainer")
      Prelude.<*> (x Data..@? "OperatingSystemName")
      Prelude.<*> (x Data..@? "OperatingSystemVersion")
      Prelude.<*> (x Data..@? "PlatformArn")
      Prelude.<*> (x Data..@? "PlatformBranchLifecycleState")
      Prelude.<*> (x Data..@? "PlatformBranchName")
      Prelude.<*> (x Data..@? "PlatformCategory")
      Prelude.<*> (x Data..@? "PlatformLifecycleState")
      Prelude.<*> (x Data..@? "PlatformName")
      Prelude.<*> (x Data..@? "PlatformOwner")
      Prelude.<*> (x Data..@? "PlatformStatus")
      Prelude.<*> (x Data..@? "PlatformVersion")
      Prelude.<*> ( x
                      Data..@? "ProgrammingLanguages"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "SolutionStackName")
      Prelude.<*> ( x
                      Data..@? "SupportedAddonList"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "SupportedTierList"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable PlatformDescription where
  hashWithSalt _salt PlatformDescription' {..} =
    _salt
      `Prelude.hashWithSalt` customAmiList
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` dateUpdated
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` frameworks
      `Prelude.hashWithSalt` maintainer
      `Prelude.hashWithSalt` operatingSystemName
      `Prelude.hashWithSalt` operatingSystemVersion
      `Prelude.hashWithSalt` platformArn
      `Prelude.hashWithSalt` platformBranchLifecycleState
      `Prelude.hashWithSalt` platformBranchName
      `Prelude.hashWithSalt` platformCategory
      `Prelude.hashWithSalt` platformLifecycleState
      `Prelude.hashWithSalt` platformName
      `Prelude.hashWithSalt` platformOwner
      `Prelude.hashWithSalt` platformStatus
      `Prelude.hashWithSalt` platformVersion
      `Prelude.hashWithSalt` programmingLanguages
      `Prelude.hashWithSalt` solutionStackName
      `Prelude.hashWithSalt` supportedAddonList
      `Prelude.hashWithSalt` supportedTierList

instance Prelude.NFData PlatformDescription where
  rnf PlatformDescription' {..} =
    Prelude.rnf customAmiList
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf dateUpdated
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf frameworks
      `Prelude.seq` Prelude.rnf maintainer
      `Prelude.seq` Prelude.rnf operatingSystemName
      `Prelude.seq` Prelude.rnf operatingSystemVersion
      `Prelude.seq` Prelude.rnf platformArn
      `Prelude.seq` Prelude.rnf platformBranchLifecycleState
      `Prelude.seq` Prelude.rnf platformBranchName
      `Prelude.seq` Prelude.rnf platformCategory
      `Prelude.seq` Prelude.rnf platformLifecycleState
      `Prelude.seq` Prelude.rnf platformName
      `Prelude.seq` Prelude.rnf platformOwner
      `Prelude.seq` Prelude.rnf platformStatus
      `Prelude.seq` Prelude.rnf platformVersion
      `Prelude.seq` Prelude.rnf programmingLanguages
      `Prelude.seq` Prelude.rnf solutionStackName
      `Prelude.seq` Prelude.rnf supportedAddonList
      `Prelude.seq` Prelude.rnf
        supportedTierList

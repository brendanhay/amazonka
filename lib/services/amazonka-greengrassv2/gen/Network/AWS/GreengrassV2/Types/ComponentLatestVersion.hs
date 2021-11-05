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
-- Module      : Network.AWS.GreengrassV2.Types.ComponentLatestVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GreengrassV2.Types.ComponentLatestVersion where

import qualified Network.AWS.Core as Core
import Network.AWS.GreengrassV2.Types.ComponentPlatform
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the latest version of a component.
--
-- /See:/ 'newComponentLatestVersion' smart constructor.
data ComponentLatestVersion = ComponentLatestVersion'
  { -- | The platforms that the component version supports.
    platforms :: Prelude.Maybe [ComponentPlatform],
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the component version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The version of the component.
    componentVersion :: Prelude.Maybe Prelude.Text,
    -- | The time at which the component was created, expressed in ISO 8601
    -- format.
    creationTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The publisher of the component version.
    publisher :: Prelude.Maybe Prelude.Text,
    -- | The description of the component version.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentLatestVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platforms', 'componentLatestVersion_platforms' - The platforms that the component version supports.
--
-- 'arn', 'componentLatestVersion_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component version.
--
-- 'componentVersion', 'componentLatestVersion_componentVersion' - The version of the component.
--
-- 'creationTimestamp', 'componentLatestVersion_creationTimestamp' - The time at which the component was created, expressed in ISO 8601
-- format.
--
-- 'publisher', 'componentLatestVersion_publisher' - The publisher of the component version.
--
-- 'description', 'componentLatestVersion_description' - The description of the component version.
newComponentLatestVersion ::
  ComponentLatestVersion
newComponentLatestVersion =
  ComponentLatestVersion'
    { platforms =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      componentVersion = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      publisher = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The platforms that the component version supports.
componentLatestVersion_platforms :: Lens.Lens' ComponentLatestVersion (Prelude.Maybe [ComponentPlatform])
componentLatestVersion_platforms = Lens.lens (\ComponentLatestVersion' {platforms} -> platforms) (\s@ComponentLatestVersion' {} a -> s {platforms = a} :: ComponentLatestVersion) Prelude.. Lens.mapping Lens.coerced

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component version.
componentLatestVersion_arn :: Lens.Lens' ComponentLatestVersion (Prelude.Maybe Prelude.Text)
componentLatestVersion_arn = Lens.lens (\ComponentLatestVersion' {arn} -> arn) (\s@ComponentLatestVersion' {} a -> s {arn = a} :: ComponentLatestVersion)

-- | The version of the component.
componentLatestVersion_componentVersion :: Lens.Lens' ComponentLatestVersion (Prelude.Maybe Prelude.Text)
componentLatestVersion_componentVersion = Lens.lens (\ComponentLatestVersion' {componentVersion} -> componentVersion) (\s@ComponentLatestVersion' {} a -> s {componentVersion = a} :: ComponentLatestVersion)

-- | The time at which the component was created, expressed in ISO 8601
-- format.
componentLatestVersion_creationTimestamp :: Lens.Lens' ComponentLatestVersion (Prelude.Maybe Prelude.UTCTime)
componentLatestVersion_creationTimestamp = Lens.lens (\ComponentLatestVersion' {creationTimestamp} -> creationTimestamp) (\s@ComponentLatestVersion' {} a -> s {creationTimestamp = a} :: ComponentLatestVersion) Prelude.. Lens.mapping Core._Time

-- | The publisher of the component version.
componentLatestVersion_publisher :: Lens.Lens' ComponentLatestVersion (Prelude.Maybe Prelude.Text)
componentLatestVersion_publisher = Lens.lens (\ComponentLatestVersion' {publisher} -> publisher) (\s@ComponentLatestVersion' {} a -> s {publisher = a} :: ComponentLatestVersion)

-- | The description of the component version.
componentLatestVersion_description :: Lens.Lens' ComponentLatestVersion (Prelude.Maybe Prelude.Text)
componentLatestVersion_description = Lens.lens (\ComponentLatestVersion' {description} -> description) (\s@ComponentLatestVersion' {} a -> s {description = a} :: ComponentLatestVersion)

instance Core.FromJSON ComponentLatestVersion where
  parseJSON =
    Core.withObject
      "ComponentLatestVersion"
      ( \x ->
          ComponentLatestVersion'
            Prelude.<$> (x Core..:? "platforms" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "componentVersion")
            Prelude.<*> (x Core..:? "creationTimestamp")
            Prelude.<*> (x Core..:? "publisher")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable ComponentLatestVersion

instance Prelude.NFData ComponentLatestVersion

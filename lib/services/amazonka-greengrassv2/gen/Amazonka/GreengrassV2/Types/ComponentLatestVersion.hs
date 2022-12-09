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
-- Module      : Amazonka.GreengrassV2.Types.ComponentLatestVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.ComponentLatestVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types.ComponentPlatform
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the latest version of a component.
--
-- /See:/ 'newComponentLatestVersion' smart constructor.
data ComponentLatestVersion = ComponentLatestVersion'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the component version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The version of the component.
    componentVersion :: Prelude.Maybe Prelude.Text,
    -- | The time at which the component was created, expressed in ISO 8601
    -- format.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The description of the component version.
    description :: Prelude.Maybe Prelude.Text,
    -- | The platforms that the component version supports.
    platforms :: Prelude.Maybe [ComponentPlatform],
    -- | The publisher of the component version.
    publisher :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'componentLatestVersion_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component version.
--
-- 'componentVersion', 'componentLatestVersion_componentVersion' - The version of the component.
--
-- 'creationTimestamp', 'componentLatestVersion_creationTimestamp' - The time at which the component was created, expressed in ISO 8601
-- format.
--
-- 'description', 'componentLatestVersion_description' - The description of the component version.
--
-- 'platforms', 'componentLatestVersion_platforms' - The platforms that the component version supports.
--
-- 'publisher', 'componentLatestVersion_publisher' - The publisher of the component version.
newComponentLatestVersion ::
  ComponentLatestVersion
newComponentLatestVersion =
  ComponentLatestVersion'
    { arn = Prelude.Nothing,
      componentVersion = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      description = Prelude.Nothing,
      platforms = Prelude.Nothing,
      publisher = Prelude.Nothing
    }

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
componentLatestVersion_creationTimestamp = Lens.lens (\ComponentLatestVersion' {creationTimestamp} -> creationTimestamp) (\s@ComponentLatestVersion' {} a -> s {creationTimestamp = a} :: ComponentLatestVersion) Prelude.. Lens.mapping Data._Time

-- | The description of the component version.
componentLatestVersion_description :: Lens.Lens' ComponentLatestVersion (Prelude.Maybe Prelude.Text)
componentLatestVersion_description = Lens.lens (\ComponentLatestVersion' {description} -> description) (\s@ComponentLatestVersion' {} a -> s {description = a} :: ComponentLatestVersion)

-- | The platforms that the component version supports.
componentLatestVersion_platforms :: Lens.Lens' ComponentLatestVersion (Prelude.Maybe [ComponentPlatform])
componentLatestVersion_platforms = Lens.lens (\ComponentLatestVersion' {platforms} -> platforms) (\s@ComponentLatestVersion' {} a -> s {platforms = a} :: ComponentLatestVersion) Prelude.. Lens.mapping Lens.coerced

-- | The publisher of the component version.
componentLatestVersion_publisher :: Lens.Lens' ComponentLatestVersion (Prelude.Maybe Prelude.Text)
componentLatestVersion_publisher = Lens.lens (\ComponentLatestVersion' {publisher} -> publisher) (\s@ComponentLatestVersion' {} a -> s {publisher = a} :: ComponentLatestVersion)

instance Data.FromJSON ComponentLatestVersion where
  parseJSON =
    Data.withObject
      "ComponentLatestVersion"
      ( \x ->
          ComponentLatestVersion'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "componentVersion")
            Prelude.<*> (x Data..:? "creationTimestamp")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "platforms" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "publisher")
      )

instance Prelude.Hashable ComponentLatestVersion where
  hashWithSalt _salt ComponentLatestVersion' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` componentVersion
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` platforms
      `Prelude.hashWithSalt` publisher

instance Prelude.NFData ComponentLatestVersion where
  rnf ComponentLatestVersion' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf componentVersion
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf platforms
      `Prelude.seq` Prelude.rnf publisher

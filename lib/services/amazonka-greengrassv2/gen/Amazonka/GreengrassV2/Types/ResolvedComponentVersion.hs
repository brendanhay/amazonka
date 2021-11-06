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
-- Module      : Amazonka.GreengrassV2.Types.ResolvedComponentVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.ResolvedComponentVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a component version that is compatible to run
-- on a Greengrass core device.
--
-- /See:/ 'newResolvedComponentVersion' smart constructor.
data ResolvedComponentVersion = ResolvedComponentVersion'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the component version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The version of the component.
    componentVersion :: Prelude.Maybe Prelude.Text,
    -- | The recipe of the component version.
    recipe :: Prelude.Maybe Core.Base64,
    -- | The name of the component.
    componentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolvedComponentVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'resolvedComponentVersion_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component version.
--
-- 'componentVersion', 'resolvedComponentVersion_componentVersion' - The version of the component.
--
-- 'recipe', 'resolvedComponentVersion_recipe' - The recipe of the component version.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'componentName', 'resolvedComponentVersion_componentName' - The name of the component.
newResolvedComponentVersion ::
  ResolvedComponentVersion
newResolvedComponentVersion =
  ResolvedComponentVersion'
    { arn = Prelude.Nothing,
      componentVersion = Prelude.Nothing,
      recipe = Prelude.Nothing,
      componentName = Prelude.Nothing
    }

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component version.
resolvedComponentVersion_arn :: Lens.Lens' ResolvedComponentVersion (Prelude.Maybe Prelude.Text)
resolvedComponentVersion_arn = Lens.lens (\ResolvedComponentVersion' {arn} -> arn) (\s@ResolvedComponentVersion' {} a -> s {arn = a} :: ResolvedComponentVersion)

-- | The version of the component.
resolvedComponentVersion_componentVersion :: Lens.Lens' ResolvedComponentVersion (Prelude.Maybe Prelude.Text)
resolvedComponentVersion_componentVersion = Lens.lens (\ResolvedComponentVersion' {componentVersion} -> componentVersion) (\s@ResolvedComponentVersion' {} a -> s {componentVersion = a} :: ResolvedComponentVersion)

-- | The recipe of the component version.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
resolvedComponentVersion_recipe :: Lens.Lens' ResolvedComponentVersion (Prelude.Maybe Prelude.ByteString)
resolvedComponentVersion_recipe = Lens.lens (\ResolvedComponentVersion' {recipe} -> recipe) (\s@ResolvedComponentVersion' {} a -> s {recipe = a} :: ResolvedComponentVersion) Prelude.. Lens.mapping Core._Base64

-- | The name of the component.
resolvedComponentVersion_componentName :: Lens.Lens' ResolvedComponentVersion (Prelude.Maybe Prelude.Text)
resolvedComponentVersion_componentName = Lens.lens (\ResolvedComponentVersion' {componentName} -> componentName) (\s@ResolvedComponentVersion' {} a -> s {componentName = a} :: ResolvedComponentVersion)

instance Core.FromJSON ResolvedComponentVersion where
  parseJSON =
    Core.withObject
      "ResolvedComponentVersion"
      ( \x ->
          ResolvedComponentVersion'
            Prelude.<$> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "componentVersion")
            Prelude.<*> (x Core..:? "recipe")
            Prelude.<*> (x Core..:? "componentName")
      )

instance Prelude.Hashable ResolvedComponentVersion

instance Prelude.NFData ResolvedComponentVersion

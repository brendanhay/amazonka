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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.ResolvedComponentVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types.VendorGuidance
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a component version that is compatible to run
-- on a Greengrass core device.
--
-- /See:/ 'newResolvedComponentVersion' smart constructor.
data ResolvedComponentVersion = ResolvedComponentVersion'
  { -- | A message that communicates details about the vendor guidance state of
    -- the component version. This message communicates why a component version
    -- is discontinued or deleted.
    message :: Prelude.Maybe Prelude.Text,
    -- | The vendor guidance state for the component version. This state
    -- indicates whether the component version has any issues that you should
    -- consider before you deploy it. The vendor guidance state can be:
    --
    -- -   @ACTIVE@ – This component version is available and recommended for
    --     use.
    --
    -- -   @DISCONTINUED@ – This component version has been discontinued by its
    --     publisher. You can deploy this component version, but we recommend
    --     that you use a different version of this component.
    --
    -- -   @DELETED@ – This component version has been deleted by its
    --     publisher, so you can\'t deploy it. If you have any existing
    --     deployments that specify this component version, those deployments
    --     will fail.
    vendorGuidance :: Prelude.Maybe VendorGuidance,
    -- | The version of the component.
    componentVersion :: Prelude.Maybe Prelude.Text,
    -- | The recipe of the component version.
    recipe :: Prelude.Maybe Data.Base64,
    -- | The name of the component.
    componentName :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the component version.
    arn :: Prelude.Maybe Prelude.Text
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
-- 'message', 'resolvedComponentVersion_message' - A message that communicates details about the vendor guidance state of
-- the component version. This message communicates why a component version
-- is discontinued or deleted.
--
-- 'vendorGuidance', 'resolvedComponentVersion_vendorGuidance' - The vendor guidance state for the component version. This state
-- indicates whether the component version has any issues that you should
-- consider before you deploy it. The vendor guidance state can be:
--
-- -   @ACTIVE@ – This component version is available and recommended for
--     use.
--
-- -   @DISCONTINUED@ – This component version has been discontinued by its
--     publisher. You can deploy this component version, but we recommend
--     that you use a different version of this component.
--
-- -   @DELETED@ – This component version has been deleted by its
--     publisher, so you can\'t deploy it. If you have any existing
--     deployments that specify this component version, those deployments
--     will fail.
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
--
-- 'arn', 'resolvedComponentVersion_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component version.
newResolvedComponentVersion ::
  ResolvedComponentVersion
newResolvedComponentVersion =
  ResolvedComponentVersion'
    { message =
        Prelude.Nothing,
      vendorGuidance = Prelude.Nothing,
      componentVersion = Prelude.Nothing,
      recipe = Prelude.Nothing,
      componentName = Prelude.Nothing,
      arn = Prelude.Nothing
    }

-- | A message that communicates details about the vendor guidance state of
-- the component version. This message communicates why a component version
-- is discontinued or deleted.
resolvedComponentVersion_message :: Lens.Lens' ResolvedComponentVersion (Prelude.Maybe Prelude.Text)
resolvedComponentVersion_message = Lens.lens (\ResolvedComponentVersion' {message} -> message) (\s@ResolvedComponentVersion' {} a -> s {message = a} :: ResolvedComponentVersion)

-- | The vendor guidance state for the component version. This state
-- indicates whether the component version has any issues that you should
-- consider before you deploy it. The vendor guidance state can be:
--
-- -   @ACTIVE@ – This component version is available and recommended for
--     use.
--
-- -   @DISCONTINUED@ – This component version has been discontinued by its
--     publisher. You can deploy this component version, but we recommend
--     that you use a different version of this component.
--
-- -   @DELETED@ – This component version has been deleted by its
--     publisher, so you can\'t deploy it. If you have any existing
--     deployments that specify this component version, those deployments
--     will fail.
resolvedComponentVersion_vendorGuidance :: Lens.Lens' ResolvedComponentVersion (Prelude.Maybe VendorGuidance)
resolvedComponentVersion_vendorGuidance = Lens.lens (\ResolvedComponentVersion' {vendorGuidance} -> vendorGuidance) (\s@ResolvedComponentVersion' {} a -> s {vendorGuidance = a} :: ResolvedComponentVersion)

-- | The version of the component.
resolvedComponentVersion_componentVersion :: Lens.Lens' ResolvedComponentVersion (Prelude.Maybe Prelude.Text)
resolvedComponentVersion_componentVersion = Lens.lens (\ResolvedComponentVersion' {componentVersion} -> componentVersion) (\s@ResolvedComponentVersion' {} a -> s {componentVersion = a} :: ResolvedComponentVersion)

-- | The recipe of the component version.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
resolvedComponentVersion_recipe :: Lens.Lens' ResolvedComponentVersion (Prelude.Maybe Prelude.ByteString)
resolvedComponentVersion_recipe = Lens.lens (\ResolvedComponentVersion' {recipe} -> recipe) (\s@ResolvedComponentVersion' {} a -> s {recipe = a} :: ResolvedComponentVersion) Prelude.. Lens.mapping Data._Base64

-- | The name of the component.
resolvedComponentVersion_componentName :: Lens.Lens' ResolvedComponentVersion (Prelude.Maybe Prelude.Text)
resolvedComponentVersion_componentName = Lens.lens (\ResolvedComponentVersion' {componentName} -> componentName) (\s@ResolvedComponentVersion' {} a -> s {componentName = a} :: ResolvedComponentVersion)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component version.
resolvedComponentVersion_arn :: Lens.Lens' ResolvedComponentVersion (Prelude.Maybe Prelude.Text)
resolvedComponentVersion_arn = Lens.lens (\ResolvedComponentVersion' {arn} -> arn) (\s@ResolvedComponentVersion' {} a -> s {arn = a} :: ResolvedComponentVersion)

instance Data.FromJSON ResolvedComponentVersion where
  parseJSON =
    Data.withObject
      "ResolvedComponentVersion"
      ( \x ->
          ResolvedComponentVersion'
            Prelude.<$> (x Data..:? "message")
            Prelude.<*> (x Data..:? "vendorGuidance")
            Prelude.<*> (x Data..:? "componentVersion")
            Prelude.<*> (x Data..:? "recipe")
            Prelude.<*> (x Data..:? "componentName")
            Prelude.<*> (x Data..:? "arn")
      )

instance Prelude.Hashable ResolvedComponentVersion where
  hashWithSalt _salt ResolvedComponentVersion' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` vendorGuidance
      `Prelude.hashWithSalt` componentVersion
      `Prelude.hashWithSalt` recipe
      `Prelude.hashWithSalt` componentName
      `Prelude.hashWithSalt` arn

instance Prelude.NFData ResolvedComponentVersion where
  rnf ResolvedComponentVersion' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf vendorGuidance
      `Prelude.seq` Prelude.rnf componentVersion
      `Prelude.seq` Prelude.rnf recipe
      `Prelude.seq` Prelude.rnf componentName
      `Prelude.seq` Prelude.rnf arn

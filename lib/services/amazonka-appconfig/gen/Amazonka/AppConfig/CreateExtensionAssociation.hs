{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppConfig.CreateExtensionAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When you create an extension or configure an Amazon Web
-- Services-authored extension, you associate the extension with an
-- AppConfig application, environment, or configuration profile. For
-- example, you can choose to run the
-- @AppConfig deployment events to Amazon SNS@ Amazon Web Services-authored
-- extension and receive notifications on an Amazon SNS topic anytime a
-- configuration deployment is started for a specific application. Defining
-- which extension to associate with an AppConfig resource is called an
-- /extension association/. An extension association is a specified
-- relationship between an extension and an AppConfig resource, such as an
-- application or a configuration profile. For more information about
-- extensions and associations, see
-- <https://docs.aws.amazon.com/appconfig/latest/userguide/working-with-appconfig-extensions.html Working with AppConfig extensions>
-- in the /AppConfig User Guide/.
module Amazonka.AppConfig.CreateExtensionAssociation
  ( -- * Creating a Request
    CreateExtensionAssociation (..),
    newCreateExtensionAssociation,

    -- * Request Lenses
    createExtensionAssociation_extensionVersionNumber,
    createExtensionAssociation_parameters,
    createExtensionAssociation_tags,
    createExtensionAssociation_extensionIdentifier,
    createExtensionAssociation_resourceIdentifier,

    -- * Destructuring the Response
    ExtensionAssociation (..),
    newExtensionAssociation,

    -- * Response Lenses
    extensionAssociation_arn,
    extensionAssociation_extensionArn,
    extensionAssociation_extensionVersionNumber,
    extensionAssociation_id,
    extensionAssociation_parameters,
    extensionAssociation_resourceArn,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateExtensionAssociation' smart constructor.
data CreateExtensionAssociation = CreateExtensionAssociation'
  { -- | The version number of the extension. If not specified, AppConfig uses
    -- the maximum version of the extension.
    extensionVersionNumber :: Prelude.Maybe Prelude.Int,
    -- | The parameter names and values defined in the extensions. Extension
    -- parameters marked @Required@ must be entered for this field.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Adds one or more tags for the specified extension association. Tags are
    -- metadata that help you categorize resources in different ways, for
    -- example, by purpose, owner, or environment. Each tag consists of a key
    -- and an optional value, both of which you define.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name, the ID, or the Amazon Resource Name (ARN) of the extension.
    extensionIdentifier :: Prelude.Text,
    -- | The ARN of an application, configuration profile, or environment.
    resourceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateExtensionAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extensionVersionNumber', 'createExtensionAssociation_extensionVersionNumber' - The version number of the extension. If not specified, AppConfig uses
-- the maximum version of the extension.
--
-- 'parameters', 'createExtensionAssociation_parameters' - The parameter names and values defined in the extensions. Extension
-- parameters marked @Required@ must be entered for this field.
--
-- 'tags', 'createExtensionAssociation_tags' - Adds one or more tags for the specified extension association. Tags are
-- metadata that help you categorize resources in different ways, for
-- example, by purpose, owner, or environment. Each tag consists of a key
-- and an optional value, both of which you define.
--
-- 'extensionIdentifier', 'createExtensionAssociation_extensionIdentifier' - The name, the ID, or the Amazon Resource Name (ARN) of the extension.
--
-- 'resourceIdentifier', 'createExtensionAssociation_resourceIdentifier' - The ARN of an application, configuration profile, or environment.
newCreateExtensionAssociation ::
  -- | 'extensionIdentifier'
  Prelude.Text ->
  -- | 'resourceIdentifier'
  Prelude.Text ->
  CreateExtensionAssociation
newCreateExtensionAssociation
  pExtensionIdentifier_
  pResourceIdentifier_ =
    CreateExtensionAssociation'
      { extensionVersionNumber =
          Prelude.Nothing,
        parameters = Prelude.Nothing,
        tags = Prelude.Nothing,
        extensionIdentifier = pExtensionIdentifier_,
        resourceIdentifier = pResourceIdentifier_
      }

-- | The version number of the extension. If not specified, AppConfig uses
-- the maximum version of the extension.
createExtensionAssociation_extensionVersionNumber :: Lens.Lens' CreateExtensionAssociation (Prelude.Maybe Prelude.Int)
createExtensionAssociation_extensionVersionNumber = Lens.lens (\CreateExtensionAssociation' {extensionVersionNumber} -> extensionVersionNumber) (\s@CreateExtensionAssociation' {} a -> s {extensionVersionNumber = a} :: CreateExtensionAssociation)

-- | The parameter names and values defined in the extensions. Extension
-- parameters marked @Required@ must be entered for this field.
createExtensionAssociation_parameters :: Lens.Lens' CreateExtensionAssociation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createExtensionAssociation_parameters = Lens.lens (\CreateExtensionAssociation' {parameters} -> parameters) (\s@CreateExtensionAssociation' {} a -> s {parameters = a} :: CreateExtensionAssociation) Prelude.. Lens.mapping Lens.coerced

-- | Adds one or more tags for the specified extension association. Tags are
-- metadata that help you categorize resources in different ways, for
-- example, by purpose, owner, or environment. Each tag consists of a key
-- and an optional value, both of which you define.
createExtensionAssociation_tags :: Lens.Lens' CreateExtensionAssociation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createExtensionAssociation_tags = Lens.lens (\CreateExtensionAssociation' {tags} -> tags) (\s@CreateExtensionAssociation' {} a -> s {tags = a} :: CreateExtensionAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The name, the ID, or the Amazon Resource Name (ARN) of the extension.
createExtensionAssociation_extensionIdentifier :: Lens.Lens' CreateExtensionAssociation Prelude.Text
createExtensionAssociation_extensionIdentifier = Lens.lens (\CreateExtensionAssociation' {extensionIdentifier} -> extensionIdentifier) (\s@CreateExtensionAssociation' {} a -> s {extensionIdentifier = a} :: CreateExtensionAssociation)

-- | The ARN of an application, configuration profile, or environment.
createExtensionAssociation_resourceIdentifier :: Lens.Lens' CreateExtensionAssociation Prelude.Text
createExtensionAssociation_resourceIdentifier = Lens.lens (\CreateExtensionAssociation' {resourceIdentifier} -> resourceIdentifier) (\s@CreateExtensionAssociation' {} a -> s {resourceIdentifier = a} :: CreateExtensionAssociation)

instance Core.AWSRequest CreateExtensionAssociation where
  type
    AWSResponse CreateExtensionAssociation =
      ExtensionAssociation
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateExtensionAssociation where
  hashWithSalt _salt CreateExtensionAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` extensionVersionNumber
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` extensionIdentifier
      `Prelude.hashWithSalt` resourceIdentifier

instance Prelude.NFData CreateExtensionAssociation where
  rnf CreateExtensionAssociation' {..} =
    Prelude.rnf extensionVersionNumber `Prelude.seq`
      Prelude.rnf parameters `Prelude.seq`
        Prelude.rnf tags `Prelude.seq`
          Prelude.rnf extensionIdentifier `Prelude.seq`
            Prelude.rnf resourceIdentifier

instance Data.ToHeaders CreateExtensionAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateExtensionAssociation where
  toJSON CreateExtensionAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExtensionVersionNumber" Data..=)
              Prelude.<$> extensionVersionNumber,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("ExtensionIdentifier" Data..= extensionIdentifier),
            Prelude.Just
              ("ResourceIdentifier" Data..= resourceIdentifier)
          ]
      )

instance Data.ToPath CreateExtensionAssociation where
  toPath = Prelude.const "/extensionassociations"

instance Data.ToQuery CreateExtensionAssociation where
  toQuery = Prelude.const Prelude.mempty

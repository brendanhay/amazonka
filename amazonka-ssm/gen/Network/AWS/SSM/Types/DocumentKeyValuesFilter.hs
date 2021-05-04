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
-- Module      : Network.AWS.SSM.Types.DocumentKeyValuesFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentKeyValuesFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | One or more filters. Use a filter to return a more specific list of
-- documents.
--
-- For keys, you can specify one or more tags that have been applied to a
-- document.
--
-- You can also use AWS-provided keys, some of which have specific allowed
-- values. These keys and their associated values are as follows:
--
-- [DocumentType]
--     -   ApplicationConfiguration
--
--     -   ApplicationConfigurationSchema
--
--     -   Automation
--
--     -   ChangeCalendar
--
--     -   Command
--
--     -   DeploymentStrategy
--
--     -   Package
--
--     -   Policy
--
--     -   Session
--
-- [Owner]
--     Note that only one @Owner@ can be specified in a request. For
--     example: @Key=Owner,Values=Self@.
--
--     -   Amazon
--
--     -   Private
--
--     -   Public
--
--     -   Self
--
--     -   ThirdParty
--
-- [PlatformTypes]
--     -   Linux
--
--     -   Windows
--
-- @Name@ is another AWS-provided key. If you use @Name@ as a key, you can
-- use a name prefix to return a list of documents. For example, in the AWS
-- CLI, to return a list of all documents that begin with @Te@, run the
-- following command:
--
-- @aws ssm list-documents --filters Key=Name,Values=Te@
--
-- You can also use the @TargetType@ AWS-provided key. For a list of valid
-- resource type values that can be used with this key, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference>
-- in the /AWS CloudFormation User Guide/.
--
-- If you specify more than two keys, only documents that are identified by
-- all the tags are returned in the results. If you specify more than two
-- values for a key, documents that are identified by any of the values are
-- returned in the results.
--
-- To specify a custom key and value pair, use the format
-- @Key=tag:tagName,Values=valueName@.
--
-- For example, if you created a key called region and are using the AWS
-- CLI to call the @list-documents@ command:
--
-- @aws ssm list-documents --filters Key=tag:region,Values=east,west Key=Owner,Values=Self@
--
-- /See:/ 'newDocumentKeyValuesFilter' smart constructor.
data DocumentKeyValuesFilter = DocumentKeyValuesFilter'
  { -- | The name of the filter key.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value for the filter key.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DocumentKeyValuesFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'documentKeyValuesFilter_key' - The name of the filter key.
--
-- 'values', 'documentKeyValuesFilter_values' - The value for the filter key.
newDocumentKeyValuesFilter ::
  DocumentKeyValuesFilter
newDocumentKeyValuesFilter =
  DocumentKeyValuesFilter'
    { key = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name of the filter key.
documentKeyValuesFilter_key :: Lens.Lens' DocumentKeyValuesFilter (Prelude.Maybe Prelude.Text)
documentKeyValuesFilter_key = Lens.lens (\DocumentKeyValuesFilter' {key} -> key) (\s@DocumentKeyValuesFilter' {} a -> s {key = a} :: DocumentKeyValuesFilter)

-- | The value for the filter key.
documentKeyValuesFilter_values :: Lens.Lens' DocumentKeyValuesFilter (Prelude.Maybe [Prelude.Text])
documentKeyValuesFilter_values = Lens.lens (\DocumentKeyValuesFilter' {values} -> values) (\s@DocumentKeyValuesFilter' {} a -> s {values = a} :: DocumentKeyValuesFilter) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.Hashable DocumentKeyValuesFilter

instance Prelude.NFData DocumentKeyValuesFilter

instance Prelude.ToJSON DocumentKeyValuesFilter where
  toJSON DocumentKeyValuesFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Key" Prelude..=) Prelude.<$> key,
            ("Values" Prelude..=) Prelude.<$> values
          ]
      )

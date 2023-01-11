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
-- Module      : Amazonka.DevOpsGuru.Types.TagCostEstimationResourceCollectionFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.TagCostEstimationResourceCollectionFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a collection of Amazon Web Services resources that are
-- identified by an Amazon Web Services tag. This collection of resources
-- is used to create a monthly cost estimate for DevOps Guru to analyze
-- Amazon Web Services resources. The maximum number of tags you can
-- specify for a cost estimate is one. The estimate created is for the cost
-- to analyze the Amazon Web Services resources defined by the tag. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacks.html Stacks>
-- in the /Amazon Web Services CloudFormation User Guide/.
--
-- /See:/ 'newTagCostEstimationResourceCollectionFilter' smart constructor.
data TagCostEstimationResourceCollectionFilter = TagCostEstimationResourceCollectionFilter'
  { -- | An Amazon Web Services tag /key/ that is used to identify the Amazon Web
    -- Services resources that DevOps Guru analyzes. All Amazon Web Services
    -- resources in your account and Region tagged with this /key/ make up your
    -- DevOps Guru application and analysis boundary.
    --
    -- The string used for a /key/ in a tag that you use to define your
    -- resource coverage must begin with the prefix @Devops-guru-@. The tag
    -- /key/ might be @DevOps-Guru-deployment-application@ or
    -- @devops-guru-rds-application@. When you create a /key/, the case of
    -- characters in the /key/ can be whatever you choose. After you create a
    -- /key/, it is case-sensitive. For example, DevOps Guru works with a /key/
    -- named @devops-guru-rds@ and a /key/ named @DevOps-Guru-RDS@, and these
    -- act as two different /keys/. Possible /key/\//value/ pairs in your
    -- application might be @Devops-Guru-production-application\/RDS@ or
    -- @Devops-Guru-production-application\/containers@.
    appBoundaryKey :: Prelude.Text,
    -- | The values in an Amazon Web Services tag collection.
    --
    -- The tag\'s /value/ is an optional field used to associate a string with
    -- the tag /key/ (for example, @111122223333@, @Production@, or a team
    -- name). The /key/ and /value/ are the tag\'s /key/ pair. Omitting the tag
    -- /value/ is the same as using an empty string. Like tag /keys/, tag
    -- /values/ are case-sensitive. You can specify a maximum of 256 characters
    -- for a tag value.
    tagValues :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagCostEstimationResourceCollectionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appBoundaryKey', 'tagCostEstimationResourceCollectionFilter_appBoundaryKey' - An Amazon Web Services tag /key/ that is used to identify the Amazon Web
-- Services resources that DevOps Guru analyzes. All Amazon Web Services
-- resources in your account and Region tagged with this /key/ make up your
-- DevOps Guru application and analysis boundary.
--
-- The string used for a /key/ in a tag that you use to define your
-- resource coverage must begin with the prefix @Devops-guru-@. The tag
-- /key/ might be @DevOps-Guru-deployment-application@ or
-- @devops-guru-rds-application@. When you create a /key/, the case of
-- characters in the /key/ can be whatever you choose. After you create a
-- /key/, it is case-sensitive. For example, DevOps Guru works with a /key/
-- named @devops-guru-rds@ and a /key/ named @DevOps-Guru-RDS@, and these
-- act as two different /keys/. Possible /key/\//value/ pairs in your
-- application might be @Devops-Guru-production-application\/RDS@ or
-- @Devops-Guru-production-application\/containers@.
--
-- 'tagValues', 'tagCostEstimationResourceCollectionFilter_tagValues' - The values in an Amazon Web Services tag collection.
--
-- The tag\'s /value/ is an optional field used to associate a string with
-- the tag /key/ (for example, @111122223333@, @Production@, or a team
-- name). The /key/ and /value/ are the tag\'s /key/ pair. Omitting the tag
-- /value/ is the same as using an empty string. Like tag /keys/, tag
-- /values/ are case-sensitive. You can specify a maximum of 256 characters
-- for a tag value.
newTagCostEstimationResourceCollectionFilter ::
  -- | 'appBoundaryKey'
  Prelude.Text ->
  -- | 'tagValues'
  Prelude.NonEmpty Prelude.Text ->
  TagCostEstimationResourceCollectionFilter
newTagCostEstimationResourceCollectionFilter
  pAppBoundaryKey_
  pTagValues_ =
    TagCostEstimationResourceCollectionFilter'
      { appBoundaryKey =
          pAppBoundaryKey_,
        tagValues =
          Lens.coerced
            Lens.# pTagValues_
      }

-- | An Amazon Web Services tag /key/ that is used to identify the Amazon Web
-- Services resources that DevOps Guru analyzes. All Amazon Web Services
-- resources in your account and Region tagged with this /key/ make up your
-- DevOps Guru application and analysis boundary.
--
-- The string used for a /key/ in a tag that you use to define your
-- resource coverage must begin with the prefix @Devops-guru-@. The tag
-- /key/ might be @DevOps-Guru-deployment-application@ or
-- @devops-guru-rds-application@. When you create a /key/, the case of
-- characters in the /key/ can be whatever you choose. After you create a
-- /key/, it is case-sensitive. For example, DevOps Guru works with a /key/
-- named @devops-guru-rds@ and a /key/ named @DevOps-Guru-RDS@, and these
-- act as two different /keys/. Possible /key/\//value/ pairs in your
-- application might be @Devops-Guru-production-application\/RDS@ or
-- @Devops-Guru-production-application\/containers@.
tagCostEstimationResourceCollectionFilter_appBoundaryKey :: Lens.Lens' TagCostEstimationResourceCollectionFilter Prelude.Text
tagCostEstimationResourceCollectionFilter_appBoundaryKey = Lens.lens (\TagCostEstimationResourceCollectionFilter' {appBoundaryKey} -> appBoundaryKey) (\s@TagCostEstimationResourceCollectionFilter' {} a -> s {appBoundaryKey = a} :: TagCostEstimationResourceCollectionFilter)

-- | The values in an Amazon Web Services tag collection.
--
-- The tag\'s /value/ is an optional field used to associate a string with
-- the tag /key/ (for example, @111122223333@, @Production@, or a team
-- name). The /key/ and /value/ are the tag\'s /key/ pair. Omitting the tag
-- /value/ is the same as using an empty string. Like tag /keys/, tag
-- /values/ are case-sensitive. You can specify a maximum of 256 characters
-- for a tag value.
tagCostEstimationResourceCollectionFilter_tagValues :: Lens.Lens' TagCostEstimationResourceCollectionFilter (Prelude.NonEmpty Prelude.Text)
tagCostEstimationResourceCollectionFilter_tagValues = Lens.lens (\TagCostEstimationResourceCollectionFilter' {tagValues} -> tagValues) (\s@TagCostEstimationResourceCollectionFilter' {} a -> s {tagValues = a} :: TagCostEstimationResourceCollectionFilter) Prelude.. Lens.coerced

instance
  Data.FromJSON
    TagCostEstimationResourceCollectionFilter
  where
  parseJSON =
    Data.withObject
      "TagCostEstimationResourceCollectionFilter"
      ( \x ->
          TagCostEstimationResourceCollectionFilter'
            Prelude.<$> (x Data..: "AppBoundaryKey")
              Prelude.<*> (x Data..: "TagValues")
      )

instance
  Prelude.Hashable
    TagCostEstimationResourceCollectionFilter
  where
  hashWithSalt
    _salt
    TagCostEstimationResourceCollectionFilter' {..} =
      _salt `Prelude.hashWithSalt` appBoundaryKey
        `Prelude.hashWithSalt` tagValues

instance
  Prelude.NFData
    TagCostEstimationResourceCollectionFilter
  where
  rnf TagCostEstimationResourceCollectionFilter' {..} =
    Prelude.rnf appBoundaryKey
      `Prelude.seq` Prelude.rnf tagValues

instance
  Data.ToJSON
    TagCostEstimationResourceCollectionFilter
  where
  toJSON TagCostEstimationResourceCollectionFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AppBoundaryKey" Data..= appBoundaryKey),
            Prelude.Just ("TagValues" Data..= tagValues)
          ]
      )

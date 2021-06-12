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
-- Module      : Network.AWS.Glue.Types.JsonClassifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JsonClassifier where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A classifier for @JSON@ content.
--
-- /See:/ 'newJsonClassifier' smart constructor.
data JsonClassifier = JsonClassifier'
  { -- | The time that this classifier was registered.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The version of this classifier.
    version :: Core.Maybe Core.Integer,
    -- | The time that this classifier was last updated.
    lastUpdated :: Core.Maybe Core.POSIX,
    -- | The name of the classifier.
    name :: Core.Text,
    -- | A @JsonPath@ string defining the JSON data for the classifier to
    -- classify. AWS Glue supports a subset of JsonPath, as described in
    -- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers>.
    jsonPath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JsonClassifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'jsonClassifier_creationTime' - The time that this classifier was registered.
--
-- 'version', 'jsonClassifier_version' - The version of this classifier.
--
-- 'lastUpdated', 'jsonClassifier_lastUpdated' - The time that this classifier was last updated.
--
-- 'name', 'jsonClassifier_name' - The name of the classifier.
--
-- 'jsonPath', 'jsonClassifier_jsonPath' - A @JsonPath@ string defining the JSON data for the classifier to
-- classify. AWS Glue supports a subset of JsonPath, as described in
-- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers>.
newJsonClassifier ::
  -- | 'name'
  Core.Text ->
  -- | 'jsonPath'
  Core.Text ->
  JsonClassifier
newJsonClassifier pName_ pJsonPath_ =
  JsonClassifier'
    { creationTime = Core.Nothing,
      version = Core.Nothing,
      lastUpdated = Core.Nothing,
      name = pName_,
      jsonPath = pJsonPath_
    }

-- | The time that this classifier was registered.
jsonClassifier_creationTime :: Lens.Lens' JsonClassifier (Core.Maybe Core.UTCTime)
jsonClassifier_creationTime = Lens.lens (\JsonClassifier' {creationTime} -> creationTime) (\s@JsonClassifier' {} a -> s {creationTime = a} :: JsonClassifier) Core.. Lens.mapping Core._Time

-- | The version of this classifier.
jsonClassifier_version :: Lens.Lens' JsonClassifier (Core.Maybe Core.Integer)
jsonClassifier_version = Lens.lens (\JsonClassifier' {version} -> version) (\s@JsonClassifier' {} a -> s {version = a} :: JsonClassifier)

-- | The time that this classifier was last updated.
jsonClassifier_lastUpdated :: Lens.Lens' JsonClassifier (Core.Maybe Core.UTCTime)
jsonClassifier_lastUpdated = Lens.lens (\JsonClassifier' {lastUpdated} -> lastUpdated) (\s@JsonClassifier' {} a -> s {lastUpdated = a} :: JsonClassifier) Core.. Lens.mapping Core._Time

-- | The name of the classifier.
jsonClassifier_name :: Lens.Lens' JsonClassifier Core.Text
jsonClassifier_name = Lens.lens (\JsonClassifier' {name} -> name) (\s@JsonClassifier' {} a -> s {name = a} :: JsonClassifier)

-- | A @JsonPath@ string defining the JSON data for the classifier to
-- classify. AWS Glue supports a subset of JsonPath, as described in
-- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers>.
jsonClassifier_jsonPath :: Lens.Lens' JsonClassifier Core.Text
jsonClassifier_jsonPath = Lens.lens (\JsonClassifier' {jsonPath} -> jsonPath) (\s@JsonClassifier' {} a -> s {jsonPath = a} :: JsonClassifier)

instance Core.FromJSON JsonClassifier where
  parseJSON =
    Core.withObject
      "JsonClassifier"
      ( \x ->
          JsonClassifier'
            Core.<$> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "LastUpdated")
            Core.<*> (x Core..: "Name")
            Core.<*> (x Core..: "JsonPath")
      )

instance Core.Hashable JsonClassifier

instance Core.NFData JsonClassifier

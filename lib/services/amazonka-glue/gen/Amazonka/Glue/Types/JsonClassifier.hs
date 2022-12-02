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
-- Module      : Amazonka.Glue.Types.JsonClassifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.JsonClassifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A classifier for @JSON@ content.
--
-- /See:/ 'newJsonClassifier' smart constructor.
data JsonClassifier = JsonClassifier'
  { -- | The time that this classifier was last updated.
    lastUpdated :: Prelude.Maybe Data.POSIX,
    -- | The time that this classifier was registered.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The version of this classifier.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The name of the classifier.
    name :: Prelude.Text,
    -- | A @JsonPath@ string defining the JSON data for the classifier to
    -- classify. Glue supports a subset of JsonPath, as described in
    -- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers>.
    jsonPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JsonClassifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdated', 'jsonClassifier_lastUpdated' - The time that this classifier was last updated.
--
-- 'creationTime', 'jsonClassifier_creationTime' - The time that this classifier was registered.
--
-- 'version', 'jsonClassifier_version' - The version of this classifier.
--
-- 'name', 'jsonClassifier_name' - The name of the classifier.
--
-- 'jsonPath', 'jsonClassifier_jsonPath' - A @JsonPath@ string defining the JSON data for the classifier to
-- classify. Glue supports a subset of JsonPath, as described in
-- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers>.
newJsonClassifier ::
  -- | 'name'
  Prelude.Text ->
  -- | 'jsonPath'
  Prelude.Text ->
  JsonClassifier
newJsonClassifier pName_ pJsonPath_ =
  JsonClassifier'
    { lastUpdated = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      version = Prelude.Nothing,
      name = pName_,
      jsonPath = pJsonPath_
    }

-- | The time that this classifier was last updated.
jsonClassifier_lastUpdated :: Lens.Lens' JsonClassifier (Prelude.Maybe Prelude.UTCTime)
jsonClassifier_lastUpdated = Lens.lens (\JsonClassifier' {lastUpdated} -> lastUpdated) (\s@JsonClassifier' {} a -> s {lastUpdated = a} :: JsonClassifier) Prelude.. Lens.mapping Data._Time

-- | The time that this classifier was registered.
jsonClassifier_creationTime :: Lens.Lens' JsonClassifier (Prelude.Maybe Prelude.UTCTime)
jsonClassifier_creationTime = Lens.lens (\JsonClassifier' {creationTime} -> creationTime) (\s@JsonClassifier' {} a -> s {creationTime = a} :: JsonClassifier) Prelude.. Lens.mapping Data._Time

-- | The version of this classifier.
jsonClassifier_version :: Lens.Lens' JsonClassifier (Prelude.Maybe Prelude.Integer)
jsonClassifier_version = Lens.lens (\JsonClassifier' {version} -> version) (\s@JsonClassifier' {} a -> s {version = a} :: JsonClassifier)

-- | The name of the classifier.
jsonClassifier_name :: Lens.Lens' JsonClassifier Prelude.Text
jsonClassifier_name = Lens.lens (\JsonClassifier' {name} -> name) (\s@JsonClassifier' {} a -> s {name = a} :: JsonClassifier)

-- | A @JsonPath@ string defining the JSON data for the classifier to
-- classify. Glue supports a subset of JsonPath, as described in
-- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers>.
jsonClassifier_jsonPath :: Lens.Lens' JsonClassifier Prelude.Text
jsonClassifier_jsonPath = Lens.lens (\JsonClassifier' {jsonPath} -> jsonPath) (\s@JsonClassifier' {} a -> s {jsonPath = a} :: JsonClassifier)

instance Data.FromJSON JsonClassifier where
  parseJSON =
    Data.withObject
      "JsonClassifier"
      ( \x ->
          JsonClassifier'
            Prelude.<$> (x Data..:? "LastUpdated")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Version")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "JsonPath")
      )

instance Prelude.Hashable JsonClassifier where
  hashWithSalt _salt JsonClassifier' {..} =
    _salt `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` jsonPath

instance Prelude.NFData JsonClassifier where
  rnf JsonClassifier' {..} =
    Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf jsonPath

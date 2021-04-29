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
-- Module      : Network.AWS.Glue.Types.JsonClassifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JsonClassifier where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A classifier for @JSON@ content.
--
-- /See:/ 'newJsonClassifier' smart constructor.
data JsonClassifier = JsonClassifier'
  { -- | The time that this classifier was registered.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The version of this classifier.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The time that this classifier was last updated.
    lastUpdated :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the classifier.
    name :: Prelude.Text,
    -- | A @JsonPath@ string defining the JSON data for the classifier to
    -- classify. AWS Glue supports a subset of JsonPath, as described in
    -- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers>.
    jsonPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'jsonPath'
  Prelude.Text ->
  JsonClassifier
newJsonClassifier pName_ pJsonPath_ =
  JsonClassifier'
    { creationTime = Prelude.Nothing,
      version = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      name = pName_,
      jsonPath = pJsonPath_
    }

-- | The time that this classifier was registered.
jsonClassifier_creationTime :: Lens.Lens' JsonClassifier (Prelude.Maybe Prelude.UTCTime)
jsonClassifier_creationTime = Lens.lens (\JsonClassifier' {creationTime} -> creationTime) (\s@JsonClassifier' {} a -> s {creationTime = a} :: JsonClassifier) Prelude.. Lens.mapping Prelude._Time

-- | The version of this classifier.
jsonClassifier_version :: Lens.Lens' JsonClassifier (Prelude.Maybe Prelude.Integer)
jsonClassifier_version = Lens.lens (\JsonClassifier' {version} -> version) (\s@JsonClassifier' {} a -> s {version = a} :: JsonClassifier)

-- | The time that this classifier was last updated.
jsonClassifier_lastUpdated :: Lens.Lens' JsonClassifier (Prelude.Maybe Prelude.UTCTime)
jsonClassifier_lastUpdated = Lens.lens (\JsonClassifier' {lastUpdated} -> lastUpdated) (\s@JsonClassifier' {} a -> s {lastUpdated = a} :: JsonClassifier) Prelude.. Lens.mapping Prelude._Time

-- | The name of the classifier.
jsonClassifier_name :: Lens.Lens' JsonClassifier Prelude.Text
jsonClassifier_name = Lens.lens (\JsonClassifier' {name} -> name) (\s@JsonClassifier' {} a -> s {name = a} :: JsonClassifier)

-- | A @JsonPath@ string defining the JSON data for the classifier to
-- classify. AWS Glue supports a subset of JsonPath, as described in
-- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers>.
jsonClassifier_jsonPath :: Lens.Lens' JsonClassifier Prelude.Text
jsonClassifier_jsonPath = Lens.lens (\JsonClassifier' {jsonPath} -> jsonPath) (\s@JsonClassifier' {} a -> s {jsonPath = a} :: JsonClassifier)

instance Prelude.FromJSON JsonClassifier where
  parseJSON =
    Prelude.withObject
      "JsonClassifier"
      ( \x ->
          JsonClassifier'
            Prelude.<$> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "Version")
            Prelude.<*> (x Prelude..:? "LastUpdated")
            Prelude.<*> (x Prelude..: "Name")
            Prelude.<*> (x Prelude..: "JsonPath")
      )

instance Prelude.Hashable JsonClassifier

instance Prelude.NFData JsonClassifier

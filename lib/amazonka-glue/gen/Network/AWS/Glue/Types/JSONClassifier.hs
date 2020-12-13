{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JSONClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JSONClassifier
  ( JSONClassifier (..),

    -- * Smart constructor
    mkJSONClassifier,

    -- * Lenses
    jsoncCreationTime,
    jsoncLastUpdated,
    jsoncJSONPath,
    jsoncName,
    jsoncVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A classifier for @JSON@ content.
--
-- /See:/ 'mkJSONClassifier' smart constructor.
data JSONClassifier = JSONClassifier'
  { -- | The time that this classifier was registered.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The time that this classifier was last updated.
    lastUpdated :: Lude.Maybe Lude.Timestamp,
    -- | A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
    jsonPath :: Lude.Text,
    -- | The name of the classifier.
    name :: Lude.Text,
    -- | The version of this classifier.
    version :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JSONClassifier' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time that this classifier was registered.
-- * 'lastUpdated' - The time that this classifier was last updated.
-- * 'jsonPath' - A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
-- * 'name' - The name of the classifier.
-- * 'version' - The version of this classifier.
mkJSONClassifier ::
  -- | 'jsonPath'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  JSONClassifier
mkJSONClassifier pJSONPath_ pName_ =
  JSONClassifier'
    { creationTime = Lude.Nothing,
      lastUpdated = Lude.Nothing,
      jsonPath = pJSONPath_,
      name = pName_,
      version = Lude.Nothing
    }

-- | The time that this classifier was registered.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsoncCreationTime :: Lens.Lens' JSONClassifier (Lude.Maybe Lude.Timestamp)
jsoncCreationTime = Lens.lens (creationTime :: JSONClassifier -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: JSONClassifier)
{-# DEPRECATED jsoncCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The time that this classifier was last updated.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsoncLastUpdated :: Lens.Lens' JSONClassifier (Lude.Maybe Lude.Timestamp)
jsoncLastUpdated = Lens.lens (lastUpdated :: JSONClassifier -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdated = a} :: JSONClassifier)
{-# DEPRECATED jsoncLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
--
-- /Note:/ Consider using 'jsonPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsoncJSONPath :: Lens.Lens' JSONClassifier Lude.Text
jsoncJSONPath = Lens.lens (jsonPath :: JSONClassifier -> Lude.Text) (\s a -> s {jsonPath = a} :: JSONClassifier)
{-# DEPRECATED jsoncJSONPath "Use generic-lens or generic-optics with 'jsonPath' instead." #-}

-- | The name of the classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsoncName :: Lens.Lens' JSONClassifier Lude.Text
jsoncName = Lens.lens (name :: JSONClassifier -> Lude.Text) (\s a -> s {name = a} :: JSONClassifier)
{-# DEPRECATED jsoncName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of this classifier.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsoncVersion :: Lens.Lens' JSONClassifier (Lude.Maybe Lude.Integer)
jsoncVersion = Lens.lens (version :: JSONClassifier -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: JSONClassifier)
{-# DEPRECATED jsoncVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromJSON JSONClassifier where
  parseJSON =
    Lude.withObject
      "JSONClassifier"
      ( \x ->
          JSONClassifier'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "LastUpdated")
            Lude.<*> (x Lude..: "JsonPath")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..:? "Version")
      )

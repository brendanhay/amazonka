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
    jcCreationTime,
    jcLastUpdated,
    jcVersion,
    jcName,
    jcJSONPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A classifier for @JSON@ content.
--
-- /See:/ 'mkJSONClassifier' smart constructor.
data JSONClassifier = JSONClassifier'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    lastUpdated :: Lude.Maybe Lude.Timestamp,
    version :: Lude.Maybe Lude.Integer,
    name :: Lude.Text,
    jsonPath :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JSONClassifier' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time that this classifier was registered.
-- * 'jsonPath' - A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
-- * 'lastUpdated' - The time that this classifier was last updated.
-- * 'name' - The name of the classifier.
-- * 'version' - The version of this classifier.
mkJSONClassifier ::
  -- | 'name'
  Lude.Text ->
  -- | 'jsonPath'
  Lude.Text ->
  JSONClassifier
mkJSONClassifier pName_ pJSONPath_ =
  JSONClassifier'
    { creationTime = Lude.Nothing,
      lastUpdated = Lude.Nothing,
      version = Lude.Nothing,
      name = pName_,
      jsonPath = pJSONPath_
    }

-- | The time that this classifier was registered.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jcCreationTime :: Lens.Lens' JSONClassifier (Lude.Maybe Lude.Timestamp)
jcCreationTime = Lens.lens (creationTime :: JSONClassifier -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: JSONClassifier)
{-# DEPRECATED jcCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The time that this classifier was last updated.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jcLastUpdated :: Lens.Lens' JSONClassifier (Lude.Maybe Lude.Timestamp)
jcLastUpdated = Lens.lens (lastUpdated :: JSONClassifier -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdated = a} :: JSONClassifier)
{-# DEPRECATED jcLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | The version of this classifier.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jcVersion :: Lens.Lens' JSONClassifier (Lude.Maybe Lude.Integer)
jcVersion = Lens.lens (version :: JSONClassifier -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: JSONClassifier)
{-# DEPRECATED jcVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jcName :: Lens.Lens' JSONClassifier Lude.Text
jcName = Lens.lens (name :: JSONClassifier -> Lude.Text) (\s a -> s {name = a} :: JSONClassifier)
{-# DEPRECATED jcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
--
-- /Note:/ Consider using 'jsonPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jcJSONPath :: Lens.Lens' JSONClassifier Lude.Text
jcJSONPath = Lens.lens (jsonPath :: JSONClassifier -> Lude.Text) (\s a -> s {jsonPath = a} :: JSONClassifier)
{-# DEPRECATED jcJSONPath "Use generic-lens or generic-optics with 'jsonPath' instead." #-}

instance Lude.FromJSON JSONClassifier where
  parseJSON =
    Lude.withObject
      "JSONClassifier"
      ( \x ->
          JSONClassifier'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "LastUpdated")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..: "JsonPath")
      )

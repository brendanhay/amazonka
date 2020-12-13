{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.UpdateJSONClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.UpdateJSONClassifierRequest
  ( UpdateJSONClassifierRequest (..),

    -- * Smart constructor
    mkUpdateJSONClassifierRequest,

    -- * Lenses
    ujcrJSONPath,
    ujcrName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a JSON classifier to be updated.
--
-- /See:/ 'mkUpdateJSONClassifierRequest' smart constructor.
data UpdateJSONClassifierRequest = UpdateJSONClassifierRequest'
  { -- | A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
    jsonPath :: Lude.Maybe Lude.Text,
    -- | The name of the classifier.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateJSONClassifierRequest' with the minimum fields required to make a request.
--
-- * 'jsonPath' - A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
-- * 'name' - The name of the classifier.
mkUpdateJSONClassifierRequest ::
  -- | 'name'
  Lude.Text ->
  UpdateJSONClassifierRequest
mkUpdateJSONClassifierRequest pName_ =
  UpdateJSONClassifierRequest'
    { jsonPath = Lude.Nothing,
      name = pName_
    }

-- | A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
--
-- /Note:/ Consider using 'jsonPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujcrJSONPath :: Lens.Lens' UpdateJSONClassifierRequest (Lude.Maybe Lude.Text)
ujcrJSONPath = Lens.lens (jsonPath :: UpdateJSONClassifierRequest -> Lude.Maybe Lude.Text) (\s a -> s {jsonPath = a} :: UpdateJSONClassifierRequest)
{-# DEPRECATED ujcrJSONPath "Use generic-lens or generic-optics with 'jsonPath' instead." #-}

-- | The name of the classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujcrName :: Lens.Lens' UpdateJSONClassifierRequest Lude.Text
ujcrName = Lens.lens (name :: UpdateJSONClassifierRequest -> Lude.Text) (\s a -> s {name = a} :: UpdateJSONClassifierRequest)
{-# DEPRECATED ujcrName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToJSON UpdateJSONClassifierRequest where
  toJSON UpdateJSONClassifierRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("JsonPath" Lude..=) Lude.<$> jsonPath,
            Lude.Just ("Name" Lude..= name)
          ]
      )

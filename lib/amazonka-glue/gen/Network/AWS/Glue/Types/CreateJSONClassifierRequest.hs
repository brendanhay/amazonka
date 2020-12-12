{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CreateJSONClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CreateJSONClassifierRequest
  ( CreateJSONClassifierRequest (..),

    -- * Smart constructor
    mkCreateJSONClassifierRequest,

    -- * Lenses
    cjcrName,
    cjcrJSONPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a JSON classifier for @CreateClassifier@ to create.
--
-- /See:/ 'mkCreateJSONClassifierRequest' smart constructor.
data CreateJSONClassifierRequest = CreateJSONClassifierRequest'
  { name ::
      Lude.Text,
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

-- | Creates a value of 'CreateJSONClassifierRequest' with the minimum fields required to make a request.
--
-- * 'jsonPath' - A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
-- * 'name' - The name of the classifier.
mkCreateJSONClassifierRequest ::
  -- | 'name'
  Lude.Text ->
  -- | 'jsonPath'
  Lude.Text ->
  CreateJSONClassifierRequest
mkCreateJSONClassifierRequest pName_ pJSONPath_ =
  CreateJSONClassifierRequest'
    { name = pName_,
      jsonPath = pJSONPath_
    }

-- | The name of the classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjcrName :: Lens.Lens' CreateJSONClassifierRequest Lude.Text
cjcrName = Lens.lens (name :: CreateJSONClassifierRequest -> Lude.Text) (\s a -> s {name = a} :: CreateJSONClassifierRequest)
{-# DEPRECATED cjcrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
--
-- /Note:/ Consider using 'jsonPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjcrJSONPath :: Lens.Lens' CreateJSONClassifierRequest Lude.Text
cjcrJSONPath = Lens.lens (jsonPath :: CreateJSONClassifierRequest -> Lude.Text) (\s a -> s {jsonPath = a} :: CreateJSONClassifierRequest)
{-# DEPRECATED cjcrJSONPath "Use generic-lens or generic-optics with 'jsonPath' instead." #-}

instance Lude.ToJSON CreateJSONClassifierRequest where
  toJSON CreateJSONClassifierRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("JsonPath" Lude..= jsonPath)
          ]
      )

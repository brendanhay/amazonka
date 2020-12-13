{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Variable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Variable
  ( Variable (..),

    -- * Smart constructor
    mkVariable,

    -- * Lenses
    vOutputFileURIValue,
    vDoubleValue,
    vStringValue,
    vName,
    vDatasetContentVersionValue,
  )
where

import Network.AWS.IoTAnalytics.Types.DatasetContentVersionValue
import Network.AWS.IoTAnalytics.Types.OutputFileURIValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An instance of a variable to be passed to the @containerAction@ execution. Each variable must have a name and a value given by one of @stringValue@ , @datasetContentVersionValue@ , or @outputFileUriValue@ .
--
-- /See:/ 'mkVariable' smart constructor.
data Variable = Variable'
  { -- | The value of the variable as a structure that specifies an output file URI.
    outputFileURIValue :: Lude.Maybe OutputFileURIValue,
    -- | The value of the variable as a double (numeric).
    doubleValue :: Lude.Maybe Lude.Double,
    -- | The value of the variable as a string.
    stringValue :: Lude.Maybe Lude.Text,
    -- | The name of the variable.
    name :: Lude.Text,
    -- | The value of the variable as a structure that specifies a dataset content version.
    datasetContentVersionValue :: Lude.Maybe DatasetContentVersionValue
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Variable' with the minimum fields required to make a request.
--
-- * 'outputFileURIValue' - The value of the variable as a structure that specifies an output file URI.
-- * 'doubleValue' - The value of the variable as a double (numeric).
-- * 'stringValue' - The value of the variable as a string.
-- * 'name' - The name of the variable.
-- * 'datasetContentVersionValue' - The value of the variable as a structure that specifies a dataset content version.
mkVariable ::
  -- | 'name'
  Lude.Text ->
  Variable
mkVariable pName_ =
  Variable'
    { outputFileURIValue = Lude.Nothing,
      doubleValue = Lude.Nothing,
      stringValue = Lude.Nothing,
      name = pName_,
      datasetContentVersionValue = Lude.Nothing
    }

-- | The value of the variable as a structure that specifies an output file URI.
--
-- /Note:/ Consider using 'outputFileURIValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vOutputFileURIValue :: Lens.Lens' Variable (Lude.Maybe OutputFileURIValue)
vOutputFileURIValue = Lens.lens (outputFileURIValue :: Variable -> Lude.Maybe OutputFileURIValue) (\s a -> s {outputFileURIValue = a} :: Variable)
{-# DEPRECATED vOutputFileURIValue "Use generic-lens or generic-optics with 'outputFileURIValue' instead." #-}

-- | The value of the variable as a double (numeric).
--
-- /Note:/ Consider using 'doubleValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vDoubleValue :: Lens.Lens' Variable (Lude.Maybe Lude.Double)
vDoubleValue = Lens.lens (doubleValue :: Variable -> Lude.Maybe Lude.Double) (\s a -> s {doubleValue = a} :: Variable)
{-# DEPRECATED vDoubleValue "Use generic-lens or generic-optics with 'doubleValue' instead." #-}

-- | The value of the variable as a string.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vStringValue :: Lens.Lens' Variable (Lude.Maybe Lude.Text)
vStringValue = Lens.lens (stringValue :: Variable -> Lude.Maybe Lude.Text) (\s a -> s {stringValue = a} :: Variable)
{-# DEPRECATED vStringValue "Use generic-lens or generic-optics with 'stringValue' instead." #-}

-- | The name of the variable.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vName :: Lens.Lens' Variable Lude.Text
vName = Lens.lens (name :: Variable -> Lude.Text) (\s a -> s {name = a} :: Variable)
{-# DEPRECATED vName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value of the variable as a structure that specifies a dataset content version.
--
-- /Note:/ Consider using 'datasetContentVersionValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vDatasetContentVersionValue :: Lens.Lens' Variable (Lude.Maybe DatasetContentVersionValue)
vDatasetContentVersionValue = Lens.lens (datasetContentVersionValue :: Variable -> Lude.Maybe DatasetContentVersionValue) (\s a -> s {datasetContentVersionValue = a} :: Variable)
{-# DEPRECATED vDatasetContentVersionValue "Use generic-lens or generic-optics with 'datasetContentVersionValue' instead." #-}

instance Lude.FromJSON Variable where
  parseJSON =
    Lude.withObject
      "Variable"
      ( \x ->
          Variable'
            Lude.<$> (x Lude..:? "outputFileUriValue")
            Lude.<*> (x Lude..:? "doubleValue")
            Lude.<*> (x Lude..:? "stringValue")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..:? "datasetContentVersionValue")
      )

instance Lude.ToJSON Variable where
  toJSON Variable' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("outputFileUriValue" Lude..=) Lude.<$> outputFileURIValue,
            ("doubleValue" Lude..=) Lude.<$> doubleValue,
            ("stringValue" Lude..=) Lude.<$> stringValue,
            Lude.Just ("name" Lude..= name),
            ("datasetContentVersionValue" Lude..=)
              Lude.<$> datasetContentVersionValue
          ]
      )

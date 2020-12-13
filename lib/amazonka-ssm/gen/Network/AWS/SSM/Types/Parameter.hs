{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Parameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Parameter
  ( Parameter (..),

    -- * Smart constructor
    mkParameter,

    -- * Lenses
    pLastModifiedDate,
    pSelector,
    pARN,
    pValue,
    pSourceResult,
    pName,
    pVersion,
    pType,
    pDataType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.ParameterType

-- | An Systems Manager parameter in Parameter Store.
--
-- /See:/ 'mkParameter' smart constructor.
data Parameter = Parameter'
  { -- | Date the parameter was last changed or updated and the parameter version was created.
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | Either the version number or the label used to retrieve the parameter value. Specify selectors by using one of the following formats:
    --
    -- parameter_name:version
    -- parameter_name:label
    selector :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the parameter.
    arn :: Lude.Maybe Lude.Text,
    -- | The parameter value.
    value :: Lude.Maybe Lude.Text,
    -- | Applies to parameters that reference information in other AWS services. SourceResult is the raw result or response from the source.
    sourceResult :: Lude.Maybe Lude.Text,
    -- | The name of the parameter.
    name :: Lude.Maybe Lude.Text,
    -- | The parameter version.
    version :: Lude.Maybe Lude.Integer,
    -- | The type of parameter. Valid values include the following: @String@ , @StringList@ , and @SecureString@ .
    type' :: Lude.Maybe ParameterType,
    -- | The data type of the parameter, such as @text@ or @aws:ec2:image@ . The default is @text@ .
    dataType :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- * 'lastModifiedDate' - Date the parameter was last changed or updated and the parameter version was created.
-- * 'selector' - Either the version number or the label used to retrieve the parameter value. Specify selectors by using one of the following formats:
--
-- parameter_name:version
-- parameter_name:label
-- * 'arn' - The Amazon Resource Name (ARN) of the parameter.
-- * 'value' - The parameter value.
-- * 'sourceResult' - Applies to parameters that reference information in other AWS services. SourceResult is the raw result or response from the source.
-- * 'name' - The name of the parameter.
-- * 'version' - The parameter version.
-- * 'type'' - The type of parameter. Valid values include the following: @String@ , @StringList@ , and @SecureString@ .
-- * 'dataType' - The data type of the parameter, such as @text@ or @aws:ec2:image@ . The default is @text@ .
mkParameter ::
  Parameter
mkParameter =
  Parameter'
    { lastModifiedDate = Lude.Nothing,
      selector = Lude.Nothing,
      arn = Lude.Nothing,
      value = Lude.Nothing,
      sourceResult = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing,
      type' = Lude.Nothing,
      dataType = Lude.Nothing
    }

-- | Date the parameter was last changed or updated and the parameter version was created.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLastModifiedDate :: Lens.Lens' Parameter (Lude.Maybe Lude.Timestamp)
pLastModifiedDate = Lens.lens (lastModifiedDate :: Parameter -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: Parameter)
{-# DEPRECATED pLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Either the version number or the label used to retrieve the parameter value. Specify selectors by using one of the following formats:
--
-- parameter_name:version
-- parameter_name:label
--
-- /Note:/ Consider using 'selector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSelector :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pSelector = Lens.lens (selector :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {selector = a} :: Parameter)
{-# DEPRECATED pSelector "Use generic-lens or generic-optics with 'selector' instead." #-}

-- | The Amazon Resource Name (ARN) of the parameter.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pARN :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pARN = Lens.lens (arn :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Parameter)
{-# DEPRECATED pARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The parameter value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pValue :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pValue = Lens.lens (value :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Parameter)
{-# DEPRECATED pValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | Applies to parameters that reference information in other AWS services. SourceResult is the raw result or response from the source.
--
-- /Note:/ Consider using 'sourceResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSourceResult :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pSourceResult = Lens.lens (sourceResult :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {sourceResult = a} :: Parameter)
{-# DEPRECATED pSourceResult "Use generic-lens or generic-optics with 'sourceResult' instead." #-}

-- | The name of the parameter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pName :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pName = Lens.lens (name :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Parameter)
{-# DEPRECATED pName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The parameter version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pVersion :: Lens.Lens' Parameter (Lude.Maybe Lude.Integer)
pVersion = Lens.lens (version :: Parameter -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: Parameter)
{-# DEPRECATED pVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The type of parameter. Valid values include the following: @String@ , @StringList@ , and @SecureString@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pType :: Lens.Lens' Parameter (Lude.Maybe ParameterType)
pType = Lens.lens (type' :: Parameter -> Lude.Maybe ParameterType) (\s a -> s {type' = a} :: Parameter)
{-# DEPRECATED pType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The data type of the parameter, such as @text@ or @aws:ec2:image@ . The default is @text@ .
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDataType :: Lens.Lens' Parameter (Lude.Maybe Lude.Text)
pDataType = Lens.lens (dataType :: Parameter -> Lude.Maybe Lude.Text) (\s a -> s {dataType = a} :: Parameter)
{-# DEPRECATED pDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

instance Lude.FromJSON Parameter where
  parseJSON =
    Lude.withObject
      "Parameter"
      ( \x ->
          Parameter'
            Lude.<$> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "Selector")
            Lude.<*> (x Lude..:? "ARN")
            Lude.<*> (x Lude..:? "Value")
            Lude.<*> (x Lude..:? "SourceResult")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "DataType")
      )

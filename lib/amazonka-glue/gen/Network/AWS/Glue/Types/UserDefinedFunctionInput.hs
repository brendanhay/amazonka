{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.UserDefinedFunctionInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.UserDefinedFunctionInput
  ( UserDefinedFunctionInput (..),

    -- * Smart constructor
    mkUserDefinedFunctionInput,

    -- * Lenses
    udfiOwnerName,
    udfiResourceURIs,
    udfiFunctionName,
    udfiOwnerType,
    udfiClassName,
  )
where

import Network.AWS.Glue.Types.PrincipalType
import Network.AWS.Glue.Types.ResourceURI
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure used to create or update a user-defined function.
--
-- /See:/ 'mkUserDefinedFunctionInput' smart constructor.
data UserDefinedFunctionInput = UserDefinedFunctionInput'
  { -- | The owner of the function.
    ownerName :: Lude.Maybe Lude.Text,
    -- | The resource URIs for the function.
    resourceURIs :: Lude.Maybe [ResourceURI],
    -- | The name of the function.
    functionName :: Lude.Maybe Lude.Text,
    -- | The owner type.
    ownerType :: Lude.Maybe PrincipalType,
    -- | The Java class that contains the function code.
    className :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserDefinedFunctionInput' with the minimum fields required to make a request.
--
-- * 'ownerName' - The owner of the function.
-- * 'resourceURIs' - The resource URIs for the function.
-- * 'functionName' - The name of the function.
-- * 'ownerType' - The owner type.
-- * 'className' - The Java class that contains the function code.
mkUserDefinedFunctionInput ::
  UserDefinedFunctionInput
mkUserDefinedFunctionInput =
  UserDefinedFunctionInput'
    { ownerName = Lude.Nothing,
      resourceURIs = Lude.Nothing,
      functionName = Lude.Nothing,
      ownerType = Lude.Nothing,
      className = Lude.Nothing
    }

-- | The owner of the function.
--
-- /Note:/ Consider using 'ownerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfiOwnerName :: Lens.Lens' UserDefinedFunctionInput (Lude.Maybe Lude.Text)
udfiOwnerName = Lens.lens (ownerName :: UserDefinedFunctionInput -> Lude.Maybe Lude.Text) (\s a -> s {ownerName = a} :: UserDefinedFunctionInput)
{-# DEPRECATED udfiOwnerName "Use generic-lens or generic-optics with 'ownerName' instead." #-}

-- | The resource URIs for the function.
--
-- /Note:/ Consider using 'resourceURIs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfiResourceURIs :: Lens.Lens' UserDefinedFunctionInput (Lude.Maybe [ResourceURI])
udfiResourceURIs = Lens.lens (resourceURIs :: UserDefinedFunctionInput -> Lude.Maybe [ResourceURI]) (\s a -> s {resourceURIs = a} :: UserDefinedFunctionInput)
{-# DEPRECATED udfiResourceURIs "Use generic-lens or generic-optics with 'resourceURIs' instead." #-}

-- | The name of the function.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfiFunctionName :: Lens.Lens' UserDefinedFunctionInput (Lude.Maybe Lude.Text)
udfiFunctionName = Lens.lens (functionName :: UserDefinedFunctionInput -> Lude.Maybe Lude.Text) (\s a -> s {functionName = a} :: UserDefinedFunctionInput)
{-# DEPRECATED udfiFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | The owner type.
--
-- /Note:/ Consider using 'ownerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfiOwnerType :: Lens.Lens' UserDefinedFunctionInput (Lude.Maybe PrincipalType)
udfiOwnerType = Lens.lens (ownerType :: UserDefinedFunctionInput -> Lude.Maybe PrincipalType) (\s a -> s {ownerType = a} :: UserDefinedFunctionInput)
{-# DEPRECATED udfiOwnerType "Use generic-lens or generic-optics with 'ownerType' instead." #-}

-- | The Java class that contains the function code.
--
-- /Note:/ Consider using 'className' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfiClassName :: Lens.Lens' UserDefinedFunctionInput (Lude.Maybe Lude.Text)
udfiClassName = Lens.lens (className :: UserDefinedFunctionInput -> Lude.Maybe Lude.Text) (\s a -> s {className = a} :: UserDefinedFunctionInput)
{-# DEPRECATED udfiClassName "Use generic-lens or generic-optics with 'className' instead." #-}

instance Lude.ToJSON UserDefinedFunctionInput where
  toJSON UserDefinedFunctionInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OwnerName" Lude..=) Lude.<$> ownerName,
            ("ResourceUris" Lude..=) Lude.<$> resourceURIs,
            ("FunctionName" Lude..=) Lude.<$> functionName,
            ("OwnerType" Lude..=) Lude.<$> ownerType,
            ("ClassName" Lude..=) Lude.<$> className
          ]
      )

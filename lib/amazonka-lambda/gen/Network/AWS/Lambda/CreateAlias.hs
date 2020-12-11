{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.CreateAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html alias> for a Lambda function version. Use aliases to provide clients with a function identifier that you can update to invoke a different version.
--
-- You can also map an alias to split invocation requests between two versions. Use the @RoutingConfig@ parameter to specify a second version and the percentage of invocation requests that it receives.
module Network.AWS.Lambda.CreateAlias
  ( -- * Creating a request
    CreateAlias (..),
    mkCreateAlias,

    -- ** Request lenses
    caRoutingConfig,
    caDescription,
    caFunctionName,
    caName,
    caFunctionVersion,

    -- * Destructuring the response
    AliasConfiguration (..),
    mkAliasConfiguration,

    -- ** Response lenses
    acRoutingConfig,
    acName,
    acFunctionVersion,
    acAliasARN,
    acDescription,
    acRevisionId,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateAlias' smart constructor.
data CreateAlias = CreateAlias'
  { routingConfig ::
      Lude.Maybe AliasRoutingConfiguration,
    description :: Lude.Maybe Lude.Text,
    functionName :: Lude.Text,
    name :: Lude.Text,
    functionVersion :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAlias' with the minimum fields required to make a request.
--
-- * 'description' - A description of the alias.
-- * 'functionName' - The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
-- * 'functionVersion' - The function version that the alias invokes.
-- * 'name' - The name of the alias.
-- * 'routingConfig' - The <https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html#configuring-alias-routing routing configuration> of the alias.
mkCreateAlias ::
  -- | 'functionName'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'functionVersion'
  Lude.Text ->
  CreateAlias
mkCreateAlias pFunctionName_ pName_ pFunctionVersion_ =
  CreateAlias'
    { routingConfig = Lude.Nothing,
      description = Lude.Nothing,
      functionName = pFunctionName_,
      name = pName_,
      functionVersion = pFunctionVersion_
    }

-- | The <https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html#configuring-alias-routing routing configuration> of the alias.
--
-- /Note:/ Consider using 'routingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caRoutingConfig :: Lens.Lens' CreateAlias (Lude.Maybe AliasRoutingConfiguration)
caRoutingConfig = Lens.lens (routingConfig :: CreateAlias -> Lude.Maybe AliasRoutingConfiguration) (\s a -> s {routingConfig = a} :: CreateAlias)
{-# DEPRECATED caRoutingConfig "Use generic-lens or generic-optics with 'routingConfig' instead." #-}

-- | A description of the alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateAlias (Lude.Maybe Lude.Text)
caDescription = Lens.lens (description :: CreateAlias -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateAlias)
{-# DEPRECATED caDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caFunctionName :: Lens.Lens' CreateAlias Lude.Text
caFunctionName = Lens.lens (functionName :: CreateAlias -> Lude.Text) (\s a -> s {functionName = a} :: CreateAlias)
{-# DEPRECATED caFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | The name of the alias.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' CreateAlias Lude.Text
caName = Lens.lens (name :: CreateAlias -> Lude.Text) (\s a -> s {name = a} :: CreateAlias)
{-# DEPRECATED caName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The function version that the alias invokes.
--
-- /Note:/ Consider using 'functionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caFunctionVersion :: Lens.Lens' CreateAlias Lude.Text
caFunctionVersion = Lens.lens (functionVersion :: CreateAlias -> Lude.Text) (\s a -> s {functionVersion = a} :: CreateAlias)
{-# DEPRECATED caFunctionVersion "Use generic-lens or generic-optics with 'functionVersion' instead." #-}

instance Lude.AWSRequest CreateAlias where
  type Rs CreateAlias = AliasConfiguration
  request = Req.postJSON lambdaService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateAlias where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateAlias where
  toJSON CreateAlias' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RoutingConfig" Lude..=) Lude.<$> routingConfig,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("FunctionVersion" Lude..= functionVersion)
          ]
      )

instance Lude.ToPath CreateAlias where
  toPath CreateAlias' {..} =
    Lude.mconcat
      ["/2015-03-31/functions/", Lude.toBS functionName, "/aliases"]

instance Lude.ToQuery CreateAlias where
  toQuery = Lude.const Lude.mempty

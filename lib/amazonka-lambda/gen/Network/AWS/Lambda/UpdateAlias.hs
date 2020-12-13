{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.UpdateAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of a Lambda function <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html alias> .
module Network.AWS.Lambda.UpdateAlias
  ( -- * Creating a request
    UpdateAlias (..),
    mkUpdateAlias,

    -- ** Request lenses
    uaRoutingConfig,
    uaName,
    uaFunctionName,
    uaFunctionVersion,
    uaDescription,
    uaRevisionId,

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

-- | /See:/ 'mkUpdateAlias' smart constructor.
data UpdateAlias = UpdateAlias'
  { -- | The <https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html#configuring-alias-routing routing configuration> of the alias.
    routingConfig :: Lude.Maybe AliasRoutingConfiguration,
    -- | The name of the alias.
    name :: Lude.Text,
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
    functionName :: Lude.Text,
    -- | The function version that the alias invokes.
    functionVersion :: Lude.Maybe Lude.Text,
    -- | A description of the alias.
    description :: Lude.Maybe Lude.Text,
    -- | Only update the alias if the revision ID matches the ID that's specified. Use this option to avoid modifying an alias that has changed since you last read it.
    revisionId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAlias' with the minimum fields required to make a request.
--
-- * 'routingConfig' - The <https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html#configuring-alias-routing routing configuration> of the alias.
-- * 'name' - The name of the alias.
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
-- * 'description' - A description of the alias.
-- * 'revisionId' - Only update the alias if the revision ID matches the ID that's specified. Use this option to avoid modifying an alias that has changed since you last read it.
mkUpdateAlias ::
  -- | 'name'
  Lude.Text ->
  -- | 'functionName'
  Lude.Text ->
  UpdateAlias
mkUpdateAlias pName_ pFunctionName_ =
  UpdateAlias'
    { routingConfig = Lude.Nothing,
      name = pName_,
      functionName = pFunctionName_,
      functionVersion = Lude.Nothing,
      description = Lude.Nothing,
      revisionId = Lude.Nothing
    }

-- | The <https://docs.aws.amazon.com/lambda/latest/dg/configuration-aliases.html#configuring-alias-routing routing configuration> of the alias.
--
-- /Note:/ Consider using 'routingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaRoutingConfig :: Lens.Lens' UpdateAlias (Lude.Maybe AliasRoutingConfiguration)
uaRoutingConfig = Lens.lens (routingConfig :: UpdateAlias -> Lude.Maybe AliasRoutingConfiguration) (\s a -> s {routingConfig = a} :: UpdateAlias)
{-# DEPRECATED uaRoutingConfig "Use generic-lens or generic-optics with 'routingConfig' instead." #-}

-- | The name of the alias.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaName :: Lens.Lens' UpdateAlias Lude.Text
uaName = Lens.lens (name :: UpdateAlias -> Lude.Text) (\s a -> s {name = a} :: UpdateAlias)
{-# DEPRECATED uaName "Use generic-lens or generic-optics with 'name' instead." #-}

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
uaFunctionName :: Lens.Lens' UpdateAlias Lude.Text
uaFunctionName = Lens.lens (functionName :: UpdateAlias -> Lude.Text) (\s a -> s {functionName = a} :: UpdateAlias)
{-# DEPRECATED uaFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | The function version that the alias invokes.
--
-- /Note:/ Consider using 'functionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaFunctionVersion :: Lens.Lens' UpdateAlias (Lude.Maybe Lude.Text)
uaFunctionVersion = Lens.lens (functionVersion :: UpdateAlias -> Lude.Maybe Lude.Text) (\s a -> s {functionVersion = a} :: UpdateAlias)
{-# DEPRECATED uaFunctionVersion "Use generic-lens or generic-optics with 'functionVersion' instead." #-}

-- | A description of the alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDescription :: Lens.Lens' UpdateAlias (Lude.Maybe Lude.Text)
uaDescription = Lens.lens (description :: UpdateAlias -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateAlias)
{-# DEPRECATED uaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Only update the alias if the revision ID matches the ID that's specified. Use this option to avoid modifying an alias that has changed since you last read it.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaRevisionId :: Lens.Lens' UpdateAlias (Lude.Maybe Lude.Text)
uaRevisionId = Lens.lens (revisionId :: UpdateAlias -> Lude.Maybe Lude.Text) (\s a -> s {revisionId = a} :: UpdateAlias)
{-# DEPRECATED uaRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

instance Lude.AWSRequest UpdateAlias where
  type Rs UpdateAlias = AliasConfiguration
  request = Req.putJSON lambdaService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateAlias where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateAlias where
  toJSON UpdateAlias' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RoutingConfig" Lude..=) Lude.<$> routingConfig,
            ("FunctionVersion" Lude..=) Lude.<$> functionVersion,
            ("Description" Lude..=) Lude.<$> description,
            ("RevisionId" Lude..=) Lude.<$> revisionId
          ]
      )

instance Lude.ToPath UpdateAlias where
  toPath UpdateAlias' {..} =
    Lude.mconcat
      [ "/2015-03-31/functions/",
        Lude.toBS functionName,
        "/aliases/",
        Lude.toBS name
      ]

instance Lude.ToQuery UpdateAlias where
  toQuery = Lude.const Lude.mempty

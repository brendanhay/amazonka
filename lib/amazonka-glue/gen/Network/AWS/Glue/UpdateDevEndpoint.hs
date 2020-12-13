{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateDevEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified development endpoint.
module Network.AWS.Glue.UpdateDevEndpoint
  ( -- * Creating a request
    UpdateDevEndpoint (..),
    mkUpdateDevEndpoint,

    -- ** Request lenses
    udeAddPublicKeys,
    udeEndpointName,
    udeCustomLibraries,
    udePublicKey,
    udeDeleteArguments,
    udeDeletePublicKeys,
    udeUpdateEtlLibraries,
    udeAddArguments,

    -- * Destructuring the response
    UpdateDevEndpointResponse (..),
    mkUpdateDevEndpointResponse,

    -- ** Response lenses
    udersResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDevEndpoint' smart constructor.
data UpdateDevEndpoint = UpdateDevEndpoint'
  { -- | The list of public keys for the @DevEndpoint@ to use.
    addPublicKeys :: Lude.Maybe [Lude.Text],
    -- | The name of the @DevEndpoint@ to be updated.
    endpointName :: Lude.Text,
    -- | Custom Python or Java libraries to be loaded in the @DevEndpoint@ .
    customLibraries :: Lude.Maybe DevEndpointCustomLibraries,
    -- | The public key for the @DevEndpoint@ to use.
    publicKey :: Lude.Maybe Lude.Text,
    -- | The list of argument keys to be deleted from the map of arguments used to configure the @DevEndpoint@ .
    deleteArguments :: Lude.Maybe [Lude.Text],
    -- | The list of public keys to be deleted from the @DevEndpoint@ .
    deletePublicKeys :: Lude.Maybe [Lude.Text],
    -- | @True@ if the list of custom libraries to be loaded in the development endpoint needs to be updated, or @False@ if otherwise.
    updateEtlLibraries :: Lude.Maybe Lude.Bool,
    -- | The map of arguments to add the map of arguments used to configure the @DevEndpoint@ .
    --
    -- Valid arguments are:
    --
    --     * @"--enable-glue-datacatalog": ""@
    --
    --
    --     * @"GLUE_PYTHON_VERSION": "3"@
    --
    --
    --     * @"GLUE_PYTHON_VERSION": "2"@
    --
    --
    -- You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
    addArguments :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDevEndpoint' with the minimum fields required to make a request.
--
-- * 'addPublicKeys' - The list of public keys for the @DevEndpoint@ to use.
-- * 'endpointName' - The name of the @DevEndpoint@ to be updated.
-- * 'customLibraries' - Custom Python or Java libraries to be loaded in the @DevEndpoint@ .
-- * 'publicKey' - The public key for the @DevEndpoint@ to use.
-- * 'deleteArguments' - The list of argument keys to be deleted from the map of arguments used to configure the @DevEndpoint@ .
-- * 'deletePublicKeys' - The list of public keys to be deleted from the @DevEndpoint@ .
-- * 'updateEtlLibraries' - @True@ if the list of custom libraries to be loaded in the development endpoint needs to be updated, or @False@ if otherwise.
-- * 'addArguments' - The map of arguments to add the map of arguments used to configure the @DevEndpoint@ .
--
-- Valid arguments are:
--
--     * @"--enable-glue-datacatalog": ""@
--
--
--     * @"GLUE_PYTHON_VERSION": "3"@
--
--
--     * @"GLUE_PYTHON_VERSION": "2"@
--
--
-- You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
mkUpdateDevEndpoint ::
  -- | 'endpointName'
  Lude.Text ->
  UpdateDevEndpoint
mkUpdateDevEndpoint pEndpointName_ =
  UpdateDevEndpoint'
    { addPublicKeys = Lude.Nothing,
      endpointName = pEndpointName_,
      customLibraries = Lude.Nothing,
      publicKey = Lude.Nothing,
      deleteArguments = Lude.Nothing,
      deletePublicKeys = Lude.Nothing,
      updateEtlLibraries = Lude.Nothing,
      addArguments = Lude.Nothing
    }

-- | The list of public keys for the @DevEndpoint@ to use.
--
-- /Note:/ Consider using 'addPublicKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeAddPublicKeys :: Lens.Lens' UpdateDevEndpoint (Lude.Maybe [Lude.Text])
udeAddPublicKeys = Lens.lens (addPublicKeys :: UpdateDevEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {addPublicKeys = a} :: UpdateDevEndpoint)
{-# DEPRECATED udeAddPublicKeys "Use generic-lens or generic-optics with 'addPublicKeys' instead." #-}

-- | The name of the @DevEndpoint@ to be updated.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeEndpointName :: Lens.Lens' UpdateDevEndpoint Lude.Text
udeEndpointName = Lens.lens (endpointName :: UpdateDevEndpoint -> Lude.Text) (\s a -> s {endpointName = a} :: UpdateDevEndpoint)
{-# DEPRECATED udeEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | Custom Python or Java libraries to be loaded in the @DevEndpoint@ .
--
-- /Note:/ Consider using 'customLibraries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeCustomLibraries :: Lens.Lens' UpdateDevEndpoint (Lude.Maybe DevEndpointCustomLibraries)
udeCustomLibraries = Lens.lens (customLibraries :: UpdateDevEndpoint -> Lude.Maybe DevEndpointCustomLibraries) (\s a -> s {customLibraries = a} :: UpdateDevEndpoint)
{-# DEPRECATED udeCustomLibraries "Use generic-lens or generic-optics with 'customLibraries' instead." #-}

-- | The public key for the @DevEndpoint@ to use.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udePublicKey :: Lens.Lens' UpdateDevEndpoint (Lude.Maybe Lude.Text)
udePublicKey = Lens.lens (publicKey :: UpdateDevEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {publicKey = a} :: UpdateDevEndpoint)
{-# DEPRECATED udePublicKey "Use generic-lens or generic-optics with 'publicKey' instead." #-}

-- | The list of argument keys to be deleted from the map of arguments used to configure the @DevEndpoint@ .
--
-- /Note:/ Consider using 'deleteArguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeDeleteArguments :: Lens.Lens' UpdateDevEndpoint (Lude.Maybe [Lude.Text])
udeDeleteArguments = Lens.lens (deleteArguments :: UpdateDevEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {deleteArguments = a} :: UpdateDevEndpoint)
{-# DEPRECATED udeDeleteArguments "Use generic-lens or generic-optics with 'deleteArguments' instead." #-}

-- | The list of public keys to be deleted from the @DevEndpoint@ .
--
-- /Note:/ Consider using 'deletePublicKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeDeletePublicKeys :: Lens.Lens' UpdateDevEndpoint (Lude.Maybe [Lude.Text])
udeDeletePublicKeys = Lens.lens (deletePublicKeys :: UpdateDevEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {deletePublicKeys = a} :: UpdateDevEndpoint)
{-# DEPRECATED udeDeletePublicKeys "Use generic-lens or generic-optics with 'deletePublicKeys' instead." #-}

-- | @True@ if the list of custom libraries to be loaded in the development endpoint needs to be updated, or @False@ if otherwise.
--
-- /Note:/ Consider using 'updateEtlLibraries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeUpdateEtlLibraries :: Lens.Lens' UpdateDevEndpoint (Lude.Maybe Lude.Bool)
udeUpdateEtlLibraries = Lens.lens (updateEtlLibraries :: UpdateDevEndpoint -> Lude.Maybe Lude.Bool) (\s a -> s {updateEtlLibraries = a} :: UpdateDevEndpoint)
{-# DEPRECATED udeUpdateEtlLibraries "Use generic-lens or generic-optics with 'updateEtlLibraries' instead." #-}

-- | The map of arguments to add the map of arguments used to configure the @DevEndpoint@ .
--
-- Valid arguments are:
--
--     * @"--enable-glue-datacatalog": ""@
--
--
--     * @"GLUE_PYTHON_VERSION": "3"@
--
--
--     * @"GLUE_PYTHON_VERSION": "2"@
--
--
-- You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
--
-- /Note:/ Consider using 'addArguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeAddArguments :: Lens.Lens' UpdateDevEndpoint (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
udeAddArguments = Lens.lens (addArguments :: UpdateDevEndpoint -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {addArguments = a} :: UpdateDevEndpoint)
{-# DEPRECATED udeAddArguments "Use generic-lens or generic-optics with 'addArguments' instead." #-}

instance Lude.AWSRequest UpdateDevEndpoint where
  type Rs UpdateDevEndpoint = UpdateDevEndpointResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateDevEndpointResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDevEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.UpdateDevEndpoint" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDevEndpoint where
  toJSON UpdateDevEndpoint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AddPublicKeys" Lude..=) Lude.<$> addPublicKeys,
            Lude.Just ("EndpointName" Lude..= endpointName),
            ("CustomLibraries" Lude..=) Lude.<$> customLibraries,
            ("PublicKey" Lude..=) Lude.<$> publicKey,
            ("DeleteArguments" Lude..=) Lude.<$> deleteArguments,
            ("DeletePublicKeys" Lude..=) Lude.<$> deletePublicKeys,
            ("UpdateEtlLibraries" Lude..=) Lude.<$> updateEtlLibraries,
            ("AddArguments" Lude..=) Lude.<$> addArguments
          ]
      )

instance Lude.ToPath UpdateDevEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDevEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDevEndpointResponse' smart constructor.
newtype UpdateDevEndpointResponse = UpdateDevEndpointResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDevEndpointResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateDevEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDevEndpointResponse
mkUpdateDevEndpointResponse pResponseStatus_ =
  UpdateDevEndpointResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udersResponseStatus :: Lens.Lens' UpdateDevEndpointResponse Lude.Int
udersResponseStatus = Lens.lens (responseStatus :: UpdateDevEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDevEndpointResponse)
{-# DEPRECATED udersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

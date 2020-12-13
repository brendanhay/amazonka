{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates mappings.
module Network.AWS.Glue.GetMapping
  ( -- * Creating a request
    GetMapping (..),
    mkGetMapping,

    -- ** Request lenses
    gmSinks,
    gmLocation,
    gmSource,

    -- * Destructuring the response
    GetMappingResponse (..),
    mkGetMappingResponse,

    -- ** Response lenses
    gmrsMapping,
    gmrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMapping' smart constructor.
data GetMapping = GetMapping'
  { -- | A list of target tables.
    sinks :: Lude.Maybe [CatalogEntry],
    -- | Parameters for the mapping.
    location :: Lude.Maybe Location,
    -- | Specifies the source table.
    source :: CatalogEntry
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMapping' with the minimum fields required to make a request.
--
-- * 'sinks' - A list of target tables.
-- * 'location' - Parameters for the mapping.
-- * 'source' - Specifies the source table.
mkGetMapping ::
  -- | 'source'
  CatalogEntry ->
  GetMapping
mkGetMapping pSource_ =
  GetMapping'
    { sinks = Lude.Nothing,
      location = Lude.Nothing,
      source = pSource_
    }

-- | A list of target tables.
--
-- /Note:/ Consider using 'sinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmSinks :: Lens.Lens' GetMapping (Lude.Maybe [CatalogEntry])
gmSinks = Lens.lens (sinks :: GetMapping -> Lude.Maybe [CatalogEntry]) (\s a -> s {sinks = a} :: GetMapping)
{-# DEPRECATED gmSinks "Use generic-lens or generic-optics with 'sinks' instead." #-}

-- | Parameters for the mapping.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmLocation :: Lens.Lens' GetMapping (Lude.Maybe Location)
gmLocation = Lens.lens (location :: GetMapping -> Lude.Maybe Location) (\s a -> s {location = a} :: GetMapping)
{-# DEPRECATED gmLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | Specifies the source table.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmSource :: Lens.Lens' GetMapping CatalogEntry
gmSource = Lens.lens (source :: GetMapping -> CatalogEntry) (\s a -> s {source = a} :: GetMapping)
{-# DEPRECATED gmSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Lude.AWSRequest GetMapping where
  type Rs GetMapping = GetMappingResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMappingResponse'
            Lude.<$> (x Lude..?> "Mapping" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMapping where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.GetMapping" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMapping where
  toJSON GetMapping' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Sinks" Lude..=) Lude.<$> sinks,
            ("Location" Lude..=) Lude.<$> location,
            Lude.Just ("Source" Lude..= source)
          ]
      )

instance Lude.ToPath GetMapping where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMapping where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMappingResponse' smart constructor.
data GetMappingResponse = GetMappingResponse'
  { -- | A list of mappings to the specified targets.
    mapping :: [MappingEntry],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMappingResponse' with the minimum fields required to make a request.
--
-- * 'mapping' - A list of mappings to the specified targets.
-- * 'responseStatus' - The response status code.
mkGetMappingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMappingResponse
mkGetMappingResponse pResponseStatus_ =
  GetMappingResponse'
    { mapping = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A list of mappings to the specified targets.
--
-- /Note:/ Consider using 'mapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrsMapping :: Lens.Lens' GetMappingResponse [MappingEntry]
gmrsMapping = Lens.lens (mapping :: GetMappingResponse -> [MappingEntry]) (\s a -> s {mapping = a} :: GetMappingResponse)
{-# DEPRECATED gmrsMapping "Use generic-lens or generic-optics with 'mapping' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrsResponseStatus :: Lens.Lens' GetMappingResponse Lude.Int
gmrsResponseStatus = Lens.lens (responseStatus :: GetMappingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMappingResponse)
{-# DEPRECATED gmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

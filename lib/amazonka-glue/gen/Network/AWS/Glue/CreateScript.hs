{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateScript
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transforms a directed acyclic graph (DAG) into code.
module Network.AWS.Glue.CreateScript
  ( -- * Creating a request
    CreateScript (..),
    mkCreateScript,

    -- ** Request lenses
    csDagEdges,
    csDagNodes,
    csLanguage,

    -- * Destructuring the response
    CreateScriptResponse (..),
    mkCreateScriptResponse,

    -- ** Response lenses
    csrfrsPythonScript,
    csrfrsScalaCode,
    csrfrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateScript' smart constructor.
data CreateScript = CreateScript'
  { -- | A list of the edges in the DAG.
    dagEdges :: Core.Maybe [Types.CodeGenEdge],
    -- | A list of the nodes in the DAG.
    dagNodes :: Core.Maybe [Types.CodeGenNode],
    -- | The programming language of the resulting code from the DAG.
    language :: Core.Maybe Types.Language
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateScript' value with any optional fields omitted.
mkCreateScript ::
  CreateScript
mkCreateScript =
  CreateScript'
    { dagEdges = Core.Nothing,
      dagNodes = Core.Nothing,
      language = Core.Nothing
    }

-- | A list of the edges in the DAG.
--
-- /Note:/ Consider using 'dagEdges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDagEdges :: Lens.Lens' CreateScript (Core.Maybe [Types.CodeGenEdge])
csDagEdges = Lens.field @"dagEdges"
{-# DEPRECATED csDagEdges "Use generic-lens or generic-optics with 'dagEdges' instead." #-}

-- | A list of the nodes in the DAG.
--
-- /Note:/ Consider using 'dagNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDagNodes :: Lens.Lens' CreateScript (Core.Maybe [Types.CodeGenNode])
csDagNodes = Lens.field @"dagNodes"
{-# DEPRECATED csDagNodes "Use generic-lens or generic-optics with 'dagNodes' instead." #-}

-- | The programming language of the resulting code from the DAG.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csLanguage :: Lens.Lens' CreateScript (Core.Maybe Types.Language)
csLanguage = Lens.field @"language"
{-# DEPRECATED csLanguage "Use generic-lens or generic-optics with 'language' instead." #-}

instance Core.FromJSON CreateScript where
  toJSON CreateScript {..} =
    Core.object
      ( Core.catMaybes
          [ ("DagEdges" Core..=) Core.<$> dagEdges,
            ("DagNodes" Core..=) Core.<$> dagNodes,
            ("Language" Core..=) Core.<$> language
          ]
      )

instance Core.AWSRequest CreateScript where
  type Rs CreateScript = CreateScriptResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.CreateScript")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateScriptResponse'
            Core.<$> (x Core..:? "PythonScript")
            Core.<*> (x Core..:? "ScalaCode")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateScriptResponse' smart constructor.
data CreateScriptResponse = CreateScriptResponse'
  { -- | The Python script generated from the DAG.
    pythonScript :: Core.Maybe Types.PythonScript,
    -- | The Scala code generated from the DAG.
    scalaCode :: Core.Maybe Types.ScalaCode,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateScriptResponse' value with any optional fields omitted.
mkCreateScriptResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateScriptResponse
mkCreateScriptResponse responseStatus =
  CreateScriptResponse'
    { pythonScript = Core.Nothing,
      scalaCode = Core.Nothing,
      responseStatus
    }

-- | The Python script generated from the DAG.
--
-- /Note:/ Consider using 'pythonScript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrfrsPythonScript :: Lens.Lens' CreateScriptResponse (Core.Maybe Types.PythonScript)
csrfrsPythonScript = Lens.field @"pythonScript"
{-# DEPRECATED csrfrsPythonScript "Use generic-lens or generic-optics with 'pythonScript' instead." #-}

-- | The Scala code generated from the DAG.
--
-- /Note:/ Consider using 'scalaCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrfrsScalaCode :: Lens.Lens' CreateScriptResponse (Core.Maybe Types.ScalaCode)
csrfrsScalaCode = Lens.field @"scalaCode"
{-# DEPRECATED csrfrsScalaCode "Use generic-lens or generic-optics with 'scalaCode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrfrsResponseStatus :: Lens.Lens' CreateScriptResponse Core.Int
csrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

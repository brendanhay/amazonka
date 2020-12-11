{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    csLanguage,
    csDagNodes,

    -- * Destructuring the response
    CreateScriptResponse (..),
    mkCreateScriptResponse,

    -- ** Response lenses
    crersPythonScript,
    crersScalaCode,
    crersResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateScript' smart constructor.
data CreateScript = CreateScript'
  { dagEdges ::
      Lude.Maybe [CodeGenEdge],
    language :: Lude.Maybe Language,
    dagNodes :: Lude.Maybe [CodeGenNode]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateScript' with the minimum fields required to make a request.
--
-- * 'dagEdges' - A list of the edges in the DAG.
-- * 'dagNodes' - A list of the nodes in the DAG.
-- * 'language' - The programming language of the resulting code from the DAG.
mkCreateScript ::
  CreateScript
mkCreateScript =
  CreateScript'
    { dagEdges = Lude.Nothing,
      language = Lude.Nothing,
      dagNodes = Lude.Nothing
    }

-- | A list of the edges in the DAG.
--
-- /Note:/ Consider using 'dagEdges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDagEdges :: Lens.Lens' CreateScript (Lude.Maybe [CodeGenEdge])
csDagEdges = Lens.lens (dagEdges :: CreateScript -> Lude.Maybe [CodeGenEdge]) (\s a -> s {dagEdges = a} :: CreateScript)
{-# DEPRECATED csDagEdges "Use generic-lens or generic-optics with 'dagEdges' instead." #-}

-- | The programming language of the resulting code from the DAG.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csLanguage :: Lens.Lens' CreateScript (Lude.Maybe Language)
csLanguage = Lens.lens (language :: CreateScript -> Lude.Maybe Language) (\s a -> s {language = a} :: CreateScript)
{-# DEPRECATED csLanguage "Use generic-lens or generic-optics with 'language' instead." #-}

-- | A list of the nodes in the DAG.
--
-- /Note:/ Consider using 'dagNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDagNodes :: Lens.Lens' CreateScript (Lude.Maybe [CodeGenNode])
csDagNodes = Lens.lens (dagNodes :: CreateScript -> Lude.Maybe [CodeGenNode]) (\s a -> s {dagNodes = a} :: CreateScript)
{-# DEPRECATED csDagNodes "Use generic-lens or generic-optics with 'dagNodes' instead." #-}

instance Lude.AWSRequest CreateScript where
  type Rs CreateScript = CreateScriptResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateScriptResponse'
            Lude.<$> (x Lude..?> "PythonScript")
            Lude.<*> (x Lude..?> "ScalaCode")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateScript where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.CreateScript" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateScript where
  toJSON CreateScript' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DagEdges" Lude..=) Lude.<$> dagEdges,
            ("Language" Lude..=) Lude.<$> language,
            ("DagNodes" Lude..=) Lude.<$> dagNodes
          ]
      )

instance Lude.ToPath CreateScript where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateScript where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateScriptResponse' smart constructor.
data CreateScriptResponse = CreateScriptResponse'
  { pythonScript ::
      Lude.Maybe Lude.Text,
    scalaCode :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateScriptResponse' with the minimum fields required to make a request.
--
-- * 'pythonScript' - The Python script generated from the DAG.
-- * 'responseStatus' - The response status code.
-- * 'scalaCode' - The Scala code generated from the DAG.
mkCreateScriptResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateScriptResponse
mkCreateScriptResponse pResponseStatus_ =
  CreateScriptResponse'
    { pythonScript = Lude.Nothing,
      scalaCode = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Python script generated from the DAG.
--
-- /Note:/ Consider using 'pythonScript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crersPythonScript :: Lens.Lens' CreateScriptResponse (Lude.Maybe Lude.Text)
crersPythonScript = Lens.lens (pythonScript :: CreateScriptResponse -> Lude.Maybe Lude.Text) (\s a -> s {pythonScript = a} :: CreateScriptResponse)
{-# DEPRECATED crersPythonScript "Use generic-lens or generic-optics with 'pythonScript' instead." #-}

-- | The Scala code generated from the DAG.
--
-- /Note:/ Consider using 'scalaCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crersScalaCode :: Lens.Lens' CreateScriptResponse (Lude.Maybe Lude.Text)
crersScalaCode = Lens.lens (scalaCode :: CreateScriptResponse -> Lude.Maybe Lude.Text) (\s a -> s {scalaCode = a} :: CreateScriptResponse)
{-# DEPRECATED crersScalaCode "Use generic-lens or generic-optics with 'scalaCode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crersResponseStatus :: Lens.Lens' CreateScriptResponse Lude.Int
crersResponseStatus = Lens.lens (responseStatus :: CreateScriptResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateScriptResponse)
{-# DEPRECATED crersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

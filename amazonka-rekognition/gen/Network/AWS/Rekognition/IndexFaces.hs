{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.IndexFaces
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects faces in the input image and adds them to the specified collection.
--
--
-- Amazon Rekognition doesn't save the actual faces that are detected. Instead, the underlying detection algorithm first detects the faces in the input image. For each face, the algorithm extracts facial features into a feature vector, and stores it in the backend database. Amazon Rekognition uses feature vectors when it performs face match and search operations using the 'SearchFaces' and 'SearchFacesByImage' operations.
--
-- For more information, see Adding Faces to a Collection in the Amazon Rekognition Developer Guide.
--
-- To get the number of faces in a collection, call 'DescribeCollection' .
--
-- If you're using version 1.0 of the face detection model, @IndexFaces@ indexes the 15 largest faces in the input image. Later versions of the face detection model index the 100 largest faces in the input image.
--
-- If you're using version 4 or later of the face model, image orientation information is not returned in the @OrientationCorrection@ field.
--
-- To determine which version of the model you're using, call 'DescribeCollection' and supply the collection ID. You can also get the model version from the value of @FaceModelVersion@ in the response from @IndexFaces@
--
-- For more information, see Model Versioning in the Amazon Rekognition Developer Guide.
--
-- If you provide the optional @ExternalImageID@ for the input image you provided, Amazon Rekognition associates this ID with all faces that it detects. When you call the 'ListFaces' operation, the response returns the external ID. You can use this external image ID to create a client-side index to associate the faces with each image. You can then use the index to find all faces in an image.
--
-- You can specify the maximum number of faces to index with the @MaxFaces@ input parameter. This is useful when you want to index the largest faces in an image and don't want to index smaller faces, such as those belonging to people standing in the background.
--
-- The @QualityFilter@ input parameter allows you to filter out detected faces that don

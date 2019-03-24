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
-- Module      : Network.AWS.SSM.CreateDocument
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Systems Manager document.
--
--
-- After you create a document, you can use CreateAssociation to associate it with one or more running instances.
--
module Network.AWS.SSM.CreateDocument
    (
    -- * Creating a Request
      createDocument
    , CreateDocument
    -- * Request Lenses
    , cdDocumentType
    , cdAttachments
    , cdVersionName
    , cdTargetType
    , cdDocumentFormat
    , cdTags
    , cdContent
    , cdName

    -- * Destructuring the Response
    , createDocumentResponse
    , CreateDocumentResponse
    -- * Response Lenses
    , cdrsDocumentDescription
    , cdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'createDocument' smart constructor.
data CreateDocument = CreateDocument'
  { _cdDocumentType   :: !(Maybe DocumentType)
  , _cdAttachments    :: !(Maybe [AttachmentsSource])
  , _cdVersionName    :: !(Maybe Text)
  , _cdTargetType     :: !(Maybe Text)
  , _cdDocumentFormat :: !(Maybe DocumentFormat)
  , _cdTags           :: !(Maybe [Tag])
  , _cdContent        :: !Text
  , _cdName           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdDocumentType' - The type of document to create. Valid document types include: @Command@ , @Policy@ , @Automation@ , @Session@ , and @Package@ .
--
-- * 'cdAttachments' - A list of key and value pairs that describe attachments to a version of a document.
--
-- * 'cdVersionName' - An optional field specifying the version of the artifact you are creating with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
--
-- * 'cdTargetType' - Specify a target type to define the kinds of resources the document can run on. For example, to run a document on EC2 instances, specify the following value: /AWS::EC2::Instance. If you specify a value of '/' the document can run on all types of resources. If you don't specify a value, the document can't run on any resources. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the /AWS CloudFormation User Guide/ .
--
-- * 'cdDocumentFormat' - Specify the document format for the request. The document format can be either JSON or YAML. JSON is the default format.
--
-- * 'cdTags' - Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag an SSM document to identify the types of targets or the environment where it will run. In this case, you could specify the following key name/value pairs:     * @Key=OS,Value=Windows@      * @Key=Environment,Value=Production@
--
-- * 'cdContent' - A valid JSON or YAML string.
--
-- * 'cdName' - A name for the Systems Manager document. /Important:/ Do not use the following to begin the names of documents you create. They are reserved by AWS for use as document prefixes:     * @aws@      * @amazon@      * @amzn@
createDocument
    :: Text -- ^ 'cdContent'
    -> Text -- ^ 'cdName'
    -> CreateDocument
createDocument pContent_ pName_ =
  CreateDocument'
    { _cdDocumentType = Nothing
    , _cdAttachments = Nothing
    , _cdVersionName = Nothing
    , _cdTargetType = Nothing
    , _cdDocumentFormat = Nothing
    , _cdTags = Nothing
    , _cdContent = pContent_
    , _cdName = pName_
    }


-- | The type of document to create. Valid document types include: @Command@ , @Policy@ , @Automation@ , @Session@ , and @Package@ .
cdDocumentType :: Lens' CreateDocument (Maybe DocumentType)
cdDocumentType = lens _cdDocumentType (\ s a -> s{_cdDocumentType = a})

-- | A list of key and value pairs that describe attachments to a version of a document.
cdAttachments :: Lens' CreateDocument [AttachmentsSource]
cdAttachments = lens _cdAttachments (\ s a -> s{_cdAttachments = a}) . _Default . _Coerce

-- | An optional field specifying the version of the artifact you are creating with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
cdVersionName :: Lens' CreateDocument (Maybe Text)
cdVersionName = lens _cdVersionName (\ s a -> s{_cdVersionName = a})

-- | Specify a target type to define the kinds of resources the document can run on. For example, to run a document on EC2 instances, specify the following value: /AWS::EC2::Instance. If you specify a value of '/' the document can run on all types of resources. If you don't specify a value, the document can't run on any resources. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the /AWS CloudFormation User Guide/ .
cdTargetType :: Lens' CreateDocument (Maybe Text)
cdTargetType = lens _cdTargetType (\ s a -> s{_cdTargetType = a})

-- | Specify the document format for the request. The document format can be either JSON or YAML. JSON is the default format.
cdDocumentFormat :: Lens' CreateDocument (Maybe DocumentFormat)
cdDocumentFormat = lens _cdDocumentFormat (\ s a -> s{_cdDocumentFormat = a})

-- | Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag an SSM document to identify the types of targets or the environment where it will run. In this case, you could specify the following key name/value pairs:     * @Key=OS,Value=Windows@      * @Key=Environment,Value=Production@
cdTags :: Lens' CreateDocument [Tag]
cdTags = lens _cdTags (\ s a -> s{_cdTags = a}) . _Default . _Coerce

-- | A valid JSON or YAML string.
cdContent :: Lens' CreateDocument Text
cdContent = lens _cdContent (\ s a -> s{_cdContent = a})

-- | A name for the Systems Manager document. /Important:/ Do not use the following to begin the names of documents you create. They are reserved by AWS for use as document prefixes:     * @aws@      * @amazon@      * @amzn@
cdName :: Lens' CreateDocument Text
cdName = lens _cdName (\ s a -> s{_cdName = a})

instance AWSRequest CreateDocument where
        type Rs CreateDocument = CreateDocumentResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 CreateDocumentResponse' <$>
                   (x .?> "DocumentDescription") <*>
                     (pure (fromEnum s)))

instance Hashable CreateDocument where

instance NFData CreateDocument where

instance ToHeaders CreateDocument where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.CreateDocument" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDocument where
        toJSON CreateDocument'{..}
          = object
              (catMaybes
                 [("DocumentType" .=) <$> _cdDocumentType,
                  ("Attachments" .=) <$> _cdAttachments,
                  ("VersionName" .=) <$> _cdVersionName,
                  ("TargetType" .=) <$> _cdTargetType,
                  ("DocumentFormat" .=) <$> _cdDocumentFormat,
                  ("Tags" .=) <$> _cdTags,
                  Just ("Content" .= _cdContent),
                  Just ("Name" .= _cdName)])

instance ToPath CreateDocument where
        toPath = const "/"

instance ToQuery CreateDocument where
        toQuery = const mempty

-- | /See:/ 'createDocumentResponse' smart constructor.
data CreateDocumentResponse = CreateDocumentResponse'
  { _cdrsDocumentDescription :: !(Maybe DocumentDescription)
  , _cdrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDocumentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdrsDocumentDescription' - Information about the Systems Manager document.
--
-- * 'cdrsResponseStatus' - -- | The response status code.
createDocumentResponse
    :: Int -- ^ 'cdrsResponseStatus'
    -> CreateDocumentResponse
createDocumentResponse pResponseStatus_ =
  CreateDocumentResponse'
    {_cdrsDocumentDescription = Nothing, _cdrsResponseStatus = pResponseStatus_}


-- | Information about the Systems Manager document.
cdrsDocumentDescription :: Lens' CreateDocumentResponse (Maybe DocumentDescription)
cdrsDocumentDescription = lens _cdrsDocumentDescription (\ s a -> s{_cdrsDocumentDescription = a})

-- | -- | The response status code.
cdrsResponseStatus :: Lens' CreateDocumentResponse Int
cdrsResponseStatus = lens _cdrsResponseStatus (\ s a -> s{_cdrsResponseStatus = a})

instance NFData CreateDocumentResponse where

# Amazon Simple Systems Management Service SDK

* [Version](#version)
* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)


## Version

`1.4.1`


## Description

Simple Systems Manager (SSM) enables you to remotely manage the
configuration of your Amazon EC2 instance. Using SSM, you can run
scripts or commands using either EC2 Run Command or SSM Config. (SSM
Config is currently available only for Windows instances.)

__Run Command__

Run Command provides an on-demand experience for executing commands. You
can use pre-defined Amazon SSM documents to perform the actions listed
later in this section, or you can create your own documents. With these
documents, you can remotely configure your instances by sending commands
using the __Commands__ page in the
<http://console.aws.amazon.com/ec2/ Amazon EC2 console>,
<http://docs.aws.amazon.com/powershell/latest/reference/items/Amazon_Simple_Systems_Management_cmdlets.html AWS Tools for Windows PowerShell>,
or the
<http://docs.aws.amazon.com/cli/latest/reference/ssm/index.html AWS CLI>.

Run Command reports the status of the command execution for each
instance targeted by a command. You can also audit the command execution
to understand who executed commands, when, and what changes were made.
By switching between different SSM documents, you can quickly configure
your instances with different types of commands. To get started with Run
Command, verify that your environment meets the prerequisites for
remotely running commands on EC2 instances
(<http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/remote-commands-prereq.html Linux>
or
<http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/remote-commands-prereq.html Windows>).

__SSM Config__

SSM Config is a lightweight instance configuration solution. SSM Config
is currently only available for Windows instances. With SSM Config, you
can specify a setup configuration for your instances. SSM Config is
similar to EC2 User Data, which is another way of running one-time
scripts or applying settings during instance launch. SSM Config is an
extension of this capability. Using SSM documents, you can specify which
actions the system should perform on your instances, including which
applications to install, which AWS Directory Service directory to join,
which Microsoft PowerShell modules to install, etc. If an instance is
missing one or more of these configurations, the system makes those
changes. By default, the system checks every five minutes to see if
there is a new configuration to apply as defined in a new SSM document.
If so, the system updates the instances accordingly. In this way, you
can remotely maintain a consistent configuration baseline on your
instances. SSM Config is available using the AWS CLI or the AWS Tools
for Windows PowerShell. For more information, see
<http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-configuration-manage.html Managing Windows Instance Configuration>.

SSM Config and Run Command include the following pre-defined documents.

Amazon Pre-defined SSM Documents

Name Description Platform

AWS-RunShellScript

Run shell scripts

Linux

AWS-UpdateSSMAgent

Update the Amazon SSM agent

Linux

AWS-JoinDirectoryServiceDomain

Join an AWS Directory

Windows

AWS-RunPowerShellScript

Run PowerShell commands or scripts

Windows

AWS-UpdateEC2Config

Update the EC2Config service

Windows

AWS-ConfigureWindowsUpdate

Configure Windows Update settings

Windows

AWS-InstallApplication

Install, repair, or uninstall software using an MSI package

Windows

AWS-InstallPowerShellModule

Install PowerShell modules

Windows

AWS-ConfigureCloudWatch

Configure Amazon CloudWatch Logs to monitor applications and systems

Windows

The commands or scripts specified in SSM documents run with
administrative privilege on your instances because the Amazon SSM agent
runs as root on Linux and the EC2Config service runs in the Local System
account on Windows. If a user has permission to execute any of the
pre-defined SSM documents (any document that begins with AWS-*) then
that user also has administrator access to the instance. Delegate access
to SSM and Run Command judiciously. This becomes extremely important if
you create your own SSM documents. Amazon Web Services does not provide
guidance about how to create secure SSM documents. You create SSM
documents and delegate access to Run Command at your own risk. As a
security best practice, we recommend that you assign access to \"AWS-*\"
documents, especially the AWS-RunShellScript document on Linux and the
AWS-RunPowerShellScript document on Windows, to trusted administrators
only. You can create SSM documents for specific tasks and delegate
access to non-administrators.

Documentation is available via [Hackage](http://hackage.haskell.org/package/amazonka-ssm)
and the [AWS API Reference](https://aws.amazon.com/documentation/).

The types from this library are intended to be used with [amazonka](http://hackage.haskell.org/package/amazonka),
which provides mechanisms for specifying AuthN/AuthZ information and sending requests.

Use of lenses is required for constructing and manipulating types.
This is due to the amount of nesting of AWS types and transparency regarding
de/serialisation into more palatable Haskell values.
The provided lenses should be compatible with any of the major lens libraries
[lens](http://hackage.haskell.org/package/lens) or [lens-family-core](http://hackage.haskell.org/package/lens-family-core).

## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/amazonka/issues).

> _Note:_ this library is an auto-generated Haskell package. Please see `amazonka-gen` for more information.


## Licence

`amazonka-ssm` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).

Parts of the code are derived from AWS service descriptions, licensed under Apache 2.0.
Source files subject to this contain an additional licensing clause in their header.
